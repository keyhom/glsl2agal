#include "ir.h"
#include "ir_visitor.h"
#include "ir_optimization.h"
#include "glsl_types.h"
#include "program/hash_table.h"
#include "replaceInstruction.h"

#include <vector>

namespace {

class lower_equivalent_builtin_to_agal_visitor : public ir_hierarchical_visitor {
public:

    lower_equivalent_builtin_to_agal_visitor()
    {
        this->progress = false;
    }

    ir_visitor_status visit_leave(ir_assignment *);

    bool progress;

}; // class lower_equivalent_builtin_to_agal_visitor


} /* anonymous namespace */

bool lower_equivalent_builtin_to_agal(exec_list *instructions)
{
    lower_equivalent_builtin_to_agal_visitor v;
    visit_list_elements(&v, instructions);
    return v.progress;
}

ir_visitor_status
lower_equivalent_builtin_to_agal_visitor::visit_leave(ir_assignment *ir)
{
    void *mem_ctx = ralloc_parent(ir);
    ir_instruction *op1 = NULL, *op2 = NULL, *op3 = NULL;

    ir_expression *expr = ir->rhs->as_expression();
    if (expr)
    {
        op1 = (ir_instruction *)expr->operands[0];
        op2 = (ir_instruction *)expr->operands[1];
        op3 = (ir_instruction *)expr->operands[2];

        switch (expr->operation)
        {
            default: // ignored
                break;
            case ir_unop_sign:
            {
                if (op1->as_constant() && op1->as_constant()->is_zero())
                {
                    // sign(0) == 0
                    ir->rhs->replace_with(new (mem_ctx)ir_constant(0));
                }
                else if (op1->as_constant())
                {
                    ir_constant *c = op1->as_constant();
                    if ((c->type->is_float() && c->value.f[0] > 0.0f) ||
                        (c->type->is_integer() && c->value.i[0] > 0))
                    {
                        ir->rhs->replace_with(new (mem_ctx) ir_constant(1.0f));
                    }
                    else if ((c->type->is_float() && c->value.f[0] < 0.0f) ||
                        (c->type->is_integer() && c->value.i[0] < 0))
                    {
                        ir->rhs->replace_with(new (mem_ctx) ir_constant(-1.0f));
                    }
                }
                else
                {
                    // replace sign -> x / abs(x)
                    ir_expression *abs = new (mem_ctx) ir_expression(ir_unop_abs, op1->as_rvalue());
                    ir_expression *div = new (mem_ctx) ir_expression(ir_binop_div, op1->as_rvalue(), abs);
                    ir_assignment *tmp = new (mem_ctx) ir_assignment(ir->lhs, div);
                    ir->replace_with(tmp);
                }
                this->progress = true;
                break;
            }
            case ir_unop_floor:
            {
                // replace floor(x) == x - fract(x)
                ir_expression *expr_tmp = new (mem_ctx) ir_expression(ir_unop_fract, op1->as_rvalue());
                ir_expression *expr_tmp1 = new (mem_ctx) ir_expression(ir_binop_sub, op1->as_rvalue(), expr_tmp->as_rvalue());
                ir_assignment *assign_tmp = new (mem_ctx) ir_assignment(ir->lhs, expr_tmp1);
                ir->replace_with(assign_tmp);
                this->progress = true;
                break;
            }
            case ir_triop_clamp:
            {
                ir_constant *c1 = op2->as_constant();
                ir_constant *c2 = op3->as_constant();
                if (c1 && c2) // both constant, if clamp(x, 0, 1), then replace as saturate(x).
                {
                    if (c1->is_zero() && c2->is_one())
                    {
                        // replace with 'saturate' instead
                        expr->operation = ir_unop_saturate;
                        expr->operands[1] = NULL;
                        expr->operands[2] = NULL;
                        this->progress = true;
                    }
                }
                
                if (expr->operation == ir_triop_clamp) // others, expand clamp(x, n, m) -> max(min(x, n), m)
                {
                    ir_expression *expr_tmp = new (mem_ctx) ir_expression(ir_binop_min, op1->as_rvalue(), op2->as_rvalue());
                    ir_assignment *assign_tmp = new (mem_ctx) ir_assignment(op1->as_dereference_variable(), expr_tmp->as_rvalue());
                    ir->insert_before(assign_tmp);
                    expr_tmp = new (mem_ctx) ir_expression(ir_binop_max, op1->as_rvalue(), op3->as_rvalue());
                    assign_tmp = new (mem_ctx) ir_assignment(ir->lhs, expr_tmp->as_rvalue());
                    ir->replace_with(assign_tmp);
                    this->progress = true;
                }
                break;
            }
        }
    }

    return visit_continue;
}

