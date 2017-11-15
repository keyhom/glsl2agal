#include "glsl_optimizer.h"
#include "ast.h"
#include "glsl_parser_extras.h"
#include "glsl_parser.h"
#include "ir_optimization.h"
#include "ir_print_metal_visitor.h"
#include "ir_print_glsl_visitor.h"
#include "ir_print_agal_visitor.h"
#include "ir_expression_flattening.h"
#include "ir_print_visitor.h"
#include "ir_stats.h"
#include "loop_analysis.h"
#include "program.h"
#include "linker.h"
#include "standalone_scaffolding.h"
#include "../../agalassembler/agal.h"
#include <string>

extern "C" struct gl_shader *
_mesa_new_shader(struct gl_context *ctx, GLuint name, GLenum type);

static void DeleteShader(struct gl_context *ctx, struct gl_shader *shader)
{
	ralloc_free(shader);
}

PrintGlslMode printMode;

static void
initialize_mesa_context(struct gl_context *ctx, glslopt_target api)
{
	gl_api mesaAPI;
	switch(api)
	{
		default:
		case kGlslTargetOpenGL:
			mesaAPI = API_OPENGL_COMPAT;
			break;
		case kGlslTargetOpenGLES20:
			mesaAPI = API_OPENGLES2;
			break;
		case kGlslTargetOpenGLES30:
			mesaAPI = API_OPENGL_CORE;
			break;
		case kGlslTargetMetal:
			mesaAPI = API_OPENGL_CORE;
			break;
	}
	initialize_context_to_defaults (ctx, mesaAPI);

	switch(api)
	{
	default:
	case kGlslTargetOpenGL:
		ctx->Const.GLSLVersion = 150;
		break;
	case kGlslTargetOpenGLES20:
		ctx->Extensions.OES_standard_derivatives = true;
		ctx->Extensions.EXT_shadow_samplers = true;
		ctx->Extensions.EXT_frag_depth = true;
		ctx->Extensions.EXT_shader_framebuffer_fetch = true;
		break;
	case kGlslTargetOpenGLES30:
		ctx->Extensions.ARB_ES3_compatibility = true;
		ctx->Extensions.EXT_shader_framebuffer_fetch = true;
		break;
	case kGlslTargetMetal:
		ctx->Extensions.ARB_ES3_compatibility = true;
		ctx->Extensions.EXT_shader_framebuffer_fetch = true;
		break;
	}


   // allow high amount of texcoords
   ctx->Const.MaxTextureCoordUnits = 16;

   ctx->Const.Program[MESA_SHADER_VERTEX].MaxTextureImageUnits = 16;
   ctx->Const.Program[MESA_SHADER_FRAGMENT].MaxTextureImageUnits = 16;
   ctx->Const.Program[MESA_SHADER_GEOMETRY].MaxTextureImageUnits = 16;

   // For GLES2.0 this would be 1, but we do support GL_EXT_draw_buffers
   ctx->Const.MaxDrawBuffers = 4;

   ctx->Driver.NewShader = _mesa_new_shader;
   ctx->Driver.DeleteShader = DeleteShader;
}


struct glslopt_ctx {
	glslopt_ctx (glslopt_target target) {
		this->target = target;
		mem_ctx = ralloc_context (NULL);
		initialize_mesa_context (&mesa_ctx, target);
	}
	~glslopt_ctx() {
		ralloc_free (mem_ctx);
	}
	struct gl_context mesa_ctx;
	void* mem_ctx;
	glslopt_target target;
};

glslopt_ctx* glslopt_initialize (glslopt_target target)
{
	return new glslopt_ctx(target);
}

void glslopt_cleanup (glslopt_ctx* ctx)
{
	delete ctx;
	_mesa_destroy_shader_compiler();
}

void glslopt_set_max_unroll_iterations (glslopt_ctx* ctx, unsigned iterations)
{
	for (int i = 0; i < MESA_SHADER_STAGES; ++i)
		ctx->mesa_ctx.Const.ShaderCompilerOptions[i].MaxUnrollIterations = iterations;
}

struct glslopt_shader_var
{
	const char* name;
	glslopt_basic_type type;
	glslopt_precision prec;
	int vectorSize;
	int matrixSize;
	int arraySize;
	int location;
};

struct glslopt_shader
{
	static void* operator new(size_t size, void *ctx)
	{
		void *node;
		node = ralloc_size(ctx, size);
		assert(node != NULL);
		return node;
	}
	static void operator delete(void *node)
	{
		ralloc_free(node);
	}

	glslopt_shader ()
		: rawOutput(0)
		, optimizedOutput(0)
		, status(false)
		, uniformCount(0)
		, uniformsSize(0)
		, inputCount(0)
		, textureCount(0)
		, statsMath(0)
		, statsTex(0)
		, statsFlow(0)
	{
		infoLog = "Shader not compiled yet";

		whole_program = rzalloc (NULL, struct gl_shader_program);
		assert(whole_program != NULL);
		whole_program->InfoLog = ralloc_strdup(whole_program, "");

		whole_program->Shaders = reralloc(whole_program, whole_program->Shaders, struct gl_shader *, whole_program->NumShaders + 1);
		assert(whole_program->Shaders != NULL);

		shader = rzalloc(whole_program, gl_shader);
		whole_program->Shaders[whole_program->NumShaders] = shader;
		whole_program->NumShaders++;

		whole_program->LinkStatus = true;
	}

	~glslopt_shader()
	{
		for (unsigned i = 0; i < MESA_SHADER_STAGES; i++)
			ralloc_free(whole_program->_LinkedShaders[i]);
		ralloc_free(whole_program);
		ralloc_free(rawOutput);
		ralloc_free(optimizedOutput);
	}

	struct gl_shader_program* whole_program;
	struct gl_shader* shader;

	static const int kMaxShaderUniforms = 1024;
	static const int kMaxShaderInputs = 128;
	static const int kMaxShaderTextures = 128;
	glslopt_shader_var uniforms[kMaxShaderUniforms];
	glslopt_shader_var inputs[kMaxShaderInputs];
	glslopt_shader_var textures[kMaxShaderInputs];
	int uniformCount, uniformsSize;
	int inputCount;
	int textureCount;
	int statsMath, statsTex, statsFlow;

	char*	rawOutput;
	char*	optimizedOutput;
	const char*	infoLog;
	bool	status;
};

static inline void debug_print_ir (const char* name, exec_list* ir, _mesa_glsl_parse_state* state, void* memctx)
{
    #if 0
	printf("**** %s:\n", name);
//	_mesa_print_ir (ir, state);
	char* foobar = _mesa_print_ir_glsl(ir, state, ralloc_strdup(memctx, ""), kPrintGlslFragment);
	printf("%s\n", foobar);
	validate_ir_tree(ir);
    #endif
}


struct precision_ctx
{
	exec_list* root_ir;
	bool res;
};


static void propagate_precision_deref(ir_instruction *ir, void *data)
{
	// variable deref with undefined precision: take from variable itself
	ir_dereference_variable* der = ir->as_dereference_variable();
	if (der && der->get_precision() == glsl_precision_undefined && der->var->data.precision != glsl_precision_undefined)
	{
		der->set_precision ((glsl_precision)der->var->data.precision);
		((precision_ctx*)data)->res = true;
	}

	// array deref with undefined precision: take from array itself
	ir_dereference_array* der_arr = ir->as_dereference_array();
	if (der_arr && der_arr->get_precision() == glsl_precision_undefined && der_arr->array->get_precision() != glsl_precision_undefined)
	{
		der_arr->set_precision (der_arr->array->get_precision());
		((precision_ctx*)data)->res = true;
	}

	// swizzle with undefined precision: take from swizzle argument
	ir_swizzle* swz = ir->as_swizzle();
	if (swz && swz->get_precision() == glsl_precision_undefined && swz->val->get_precision() != glsl_precision_undefined)
	{
		swz->set_precision (swz->val->get_precision());
		((precision_ctx*)data)->res = true;
	}

}

static void propagate_precision_expr(ir_instruction *ir, void *data)
{
	ir_expression* expr = ir->as_expression();
	if (!expr)
		return;
	if (expr->get_precision() != glsl_precision_undefined)
		return;

	glsl_precision prec_params_max = glsl_precision_undefined;
	for (int i = 0; i < (int)expr->get_num_operands(); ++i)
	{
		ir_rvalue* op = expr->operands[i];
		if (op && op->get_precision() != glsl_precision_undefined)
			prec_params_max = higher_precision (prec_params_max, op->get_precision());
	}
	if (expr->get_precision() != prec_params_max)
	{
		expr->set_precision (prec_params_max);
		((precision_ctx*)data)->res = true;
	}

}

static void propagate_precision_texture(ir_instruction *ir, void *data)
{
	ir_texture* tex = ir->as_texture();
	if (!tex)
		return;

	glsl_precision sampler_prec = tex->sampler->get_precision();
	if (tex->get_precision() == sampler_prec || sampler_prec == glsl_precision_undefined)
		return;

	// set precision of ir_texture node to that of the sampler itself
	tex->set_precision(sampler_prec);
	((precision_ctx*)data)->res = true;
}

struct undefined_ass_ctx
{
	ir_variable* var;
	bool res;
};

static void has_only_undefined_precision_assignments(ir_instruction *ir, void *data)
{
	ir_assignment* ass = ir->as_assignment();
	if (!ass)
		return;
	undefined_ass_ctx* ctx = (undefined_ass_ctx*)data;
	if (ass->whole_variable_written() != ctx->var)
		return;
	glsl_precision prec = ass->rhs->get_precision();
	if (prec == glsl_precision_undefined)
		return;
	ctx->res = false;
}


static void propagate_precision_assign(ir_instruction *ir, void *data)
{
	ir_assignment* ass = ir->as_assignment();
	if (!ass || !ass->lhs || !ass->rhs)
		return;

	glsl_precision lp = ass->lhs->get_precision();
	glsl_precision rp = ass->rhs->get_precision();

	// for assignments with LHS having undefined precision, take it from RHS
	if (rp != glsl_precision_undefined)
	{
		ir_variable* lhs_var = ass->lhs->variable_referenced();
		if (lp == glsl_precision_undefined)
		{
			if (lhs_var)
				lhs_var->data.precision = rp;
			ass->lhs->set_precision (rp);
			((precision_ctx*)data)->res = true;
		}
		return;
	}

	// for assignments where LHS has precision, but RHS is a temporary variable
	// with undefined precision that's only assigned from other undefined precision
	// sources -> make the RHS variable take LHS precision
	if (lp != glsl_precision_undefined && rp == glsl_precision_undefined)
	{
		ir_dereference* deref = ass->rhs->as_dereference();
		if (deref)
		{
			ir_variable* rhs_var = deref->variable_referenced();
			if (rhs_var && rhs_var->data.mode == ir_var_temporary && rhs_var->data.precision == glsl_precision_undefined)
			{
				undefined_ass_ctx ctx;
				ctx.var = rhs_var;
				// find if we only assign to it from undefined precision sources
				ctx.res = true;
				exec_list* root_ir = ((precision_ctx*)data)->root_ir;
				foreach_in_list(ir_instruction, inst, root_ir)
				{
					visit_tree (ir, has_only_undefined_precision_assignments, &ctx);
				}
				if (ctx.res)
				{
					rhs_var->data.precision = lp;
					ass->rhs->set_precision(lp);
					((precision_ctx*)data)->res = true;
				}
			}
		}
		return;
	}
}


static void propagate_precision_call(ir_instruction *ir, void *data)
{
	ir_call* call = ir->as_call();
	if (!call)
		return;
	if (!call->return_deref)
		return;
	if (call->return_deref->get_precision() == glsl_precision_undefined /*&& call->callee->precision == glsl_precision_undefined*/)
	{
		glsl_precision prec_params_max = glsl_precision_undefined;
		foreach_two_lists(formal_node, &call->callee->parameters,
						  actual_node, &call->actual_parameters) {
			ir_variable* sig_param = (ir_variable*)formal_node;
			ir_rvalue* param = (ir_rvalue*)actual_node;

			glsl_precision p = (glsl_precision)sig_param->data.precision;
			if (p == glsl_precision_undefined)
				p = param->get_precision();

			prec_params_max = higher_precision (prec_params_max, p);
		}
		if (call->return_deref->get_precision() != prec_params_max)
		{
			call->return_deref->set_precision (prec_params_max);
			((precision_ctx*)data)->res = true;
		}
	}
}

static bool propagate_precision(exec_list* list, bool assign_high_to_undefined)
{
	bool anyProgress = false;
	precision_ctx ctx;

	do {
		ctx.res = false;
		ctx.root_ir = list;
		foreach_in_list(ir_instruction, ir, list)
		{
			visit_tree (ir, propagate_precision_texture, &ctx);
			visit_tree (ir, propagate_precision_deref, &ctx);
			bool hadProgress = ctx.res;
			ctx.res = false;
			visit_tree (ir, propagate_precision_assign, &ctx);
			if (ctx.res)
			{
				// assignment precision propagation might have added precision
				// to some variables; need to propagate dereference precision right
				// after that too.
				visit_tree (ir, propagate_precision_deref, &ctx);
			}
			ctx.res |= hadProgress;
			visit_tree (ir, propagate_precision_call, &ctx);
			visit_tree (ir, propagate_precision_expr, &ctx);
		}
		anyProgress |= ctx.res;
	} while (ctx.res);
	anyProgress |= ctx.res;

	// for globals that have undefined precision, set it to highp
	if (assign_high_to_undefined)
	{
		foreach_in_list(ir_instruction, ir, list)
		{
			ir_variable* var = ir->as_variable();
			if (var)
			{
				if (var->data.precision == glsl_precision_undefined)
				{
					var->data.precision = glsl_precision_high;
					anyProgress = true;
				}
			}
		}
	}

	return anyProgress;
}

static bool emitComma = false;

class ir_type_printing_visitor : public ir_hierarchical_visitor {
public:
	glslopt_shader *shader;
   ir_type_printing_visitor(glslopt_shader* _shader)
   {
   		shader = _shader;
   }
   virtual ir_visitor_status visit(ir_variable *);
};

ir_visitor_status
ir_type_printing_visitor::visit(ir_variable *ir)
{
   ralloc_asprintf_append (&shader->optimizedOutput, "%c\"%s\":\"%s\"\n", emitComma ? ',' : ' ', ir->name, ir->type->name);
   emitComma = true;

   return visit_continue;
}

bool
do_print_types(exec_list *instructions, glslopt_shader* shader)
{
   ir_type_printing_visitor v(shader);
   v.run(instructions);
   return false;
}

class ir_storage_printing_visitor : public ir_hierarchical_visitor {
public:
	glslopt_shader *shader;
   ir_storage_printing_visitor(glslopt_shader* _shader)
   {
   		shader = _shader;
   }
   virtual ir_visitor_status visit(ir_variable *);
};

ir_visitor_status
ir_storage_printing_visitor::visit(ir_variable *ir)
{
   ralloc_asprintf_append (&shader->optimizedOutput, "%c\"%s\":\"%s\"\n", emitComma ? ',' : ' ', ir->name, ir_variable_mode_names[ir->data.mode]);
   emitComma = true;

   return visit_continue;
}

bool
do_print_storage(exec_list *instructions, glslopt_shader* shader)
{
   ir_storage_printing_visitor v(shader);
   v.run(instructions);
   return false;
}

class ir_constant_printing_visitor : public ir_hierarchical_visitor {
public:
	glslopt_shader *shader;
	int numConsts;
   ir_constant_printing_visitor(glslopt_shader* _shader)
   {
   		numConsts = 0;
   		shader = _shader;
   }
   virtual ir_visitor_status visit(ir_variable *);
};

ir_visitor_status
ir_constant_printing_visitor::visit(ir_variable *ir)
{
	ir_constant *c = ir->constant_value;

   if(!c)
   	return visit_continue;

   if(ir->data.mode == ir_var_shader_out)
   	return visit_continue;

   if(c->type == glsl_type::vec4_type || c->type == glsl_type::vec3_type || c->type == glsl_type::vec2_type || c->type == glsl_type::float_type) {
   		int n = c->type->vector_elements;

		ralloc_asprintf_append (&shader->optimizedOutput, "%c\"%s\": [%f, %f, %f, %f]\n", emitComma ? ',' : ' ', ir->name,
		n > 0 ? ir->constant_value->get_float_component(0) : 0.0f,
		n > 1 ? ir->constant_value->get_float_component(1) : 0.0f,
		n > 2 ? ir->constant_value->get_float_component(2) : 0.0f,
		n > 3 ? ir->constant_value->get_float_component(3) : 0.0f);
		numConsts++;
   } else {
   	ralloc_asprintf_append (&shader->optimizedOutput, "%c\"%s\": \"UNHANDLED_CONST_TYPE\"\n", emitComma ? ',' : ' ', ir->name);
   }
   emitComma = true;

   return visit_continue;
}

bool
do_print_constants(exec_list *instructions, glslopt_shader* shader)
{
   ir_constant_printing_visitor v(shader);
   v.run(instructions);
   if(v.numConsts == 0) {
   	ralloc_asprintf_append (&shader->optimizedOutput, " \"%s\": [0.0, 0.0, 0.0, 0.0]\n", printMode == kPrintGlslVertex ? "vc0" : "fc0");
   }
   return false;
}


static void
print_agal_var_mapping(const void *key, void *data, void *closure) {
	char *nm = (char*)data;
	glslopt_shader* shader = (glslopt_shader*)closure;
	if(! ((nm[0] == 'v' && nm[1] == 't') || (nm[0] == 'f' && nm[1] == 't'))) {
		ralloc_asprintf_append (&shader->optimizedOutput, "%c\"%s\":\"%s\"\n", emitComma ? ',' : ' ', key, data);
		emitComma = true;
	}
}

bool glslOptimizerVerbose = false;

void dump(const char *nm, exec_list *ir, _mesa_glsl_parse_state *state, PrintGlslMode printMode, bool validate = true)
{
	if(!glslOptimizerVerbose) return;

	fprintf(stderr, "glsl %s:\n%s", nm, _mesa_print_ir_glsl(ir, state, NULL, printMode));
    fprintf(stderr, "//------------------------------------------------------------------------------\n");
	if(validate) {
		fprintf(stderr, "validate: \n"); validate_ir_tree(ir);
	}
}

static void do_optimization_passes(exec_list* ir, bool linked, _mesa_glsl_parse_state* state, void* mem_ctx, bool agal = false)
{
	bool progress;
	// FIXME: Shouldn't need to bound the number of passes
	int passes = 0,
		kMaximumPasses = 1000;
	do {
		progress = false;
		++passes;
		bool progress2;
		debug_print_ir ("Initial", ir, state, mem_ctx);
		if (linked) {
			progress2 = do_function_inlining(ir); progress |= progress2; if (progress2) debug_print_ir ("After inlining", ir, state, mem_ctx);
			progress2 = do_dead_functions(ir); progress |= progress2; if (progress2) debug_print_ir ("After dead functions", ir, state, mem_ctx);
			progress2 = do_structure_splitting(ir); progress |= progress2; if (progress2) debug_print_ir ("After struct splitting", ir, state, mem_ctx);
		}
		progress2 = do_if_simplification(ir); progress |= progress2; if (progress2) debug_print_ir ("After if simpl", ir, state, mem_ctx);
		progress2 = opt_flatten_nested_if_blocks(ir); progress |= progress2; if (progress2) debug_print_ir ("After if flatten", ir, state, mem_ctx);
		progress2 = propagate_precision (ir, state->metal_target); progress |= progress2; if (progress2) debug_print_ir ("After prec propagation", ir, state, mem_ctx);
		progress2 = do_copy_propagation(ir); progress |= progress2; if (progress2) debug_print_ir ("After copy propagation", ir, state, mem_ctx);
		progress2 = do_copy_propagation_elements(ir); progress |= progress2; if (progress2) debug_print_ir ("After copy propagation elems", ir, state, mem_ctx);

		if (linked)
		{
			progress2 = do_vectorize(ir); progress |= progress2; if (progress2) debug_print_ir ("After vectorize", ir, state, mem_ctx);
		}
		if (linked) {
			progress2 = do_dead_code(ir,false); progress |= progress2; if (progress2) debug_print_ir ("After dead code", ir, state, mem_ctx);
		} else {
			progress2 = do_dead_code_unlinked(ir); progress |= progress2; if (progress2) debug_print_ir ("After dead code unlinked", ir, state, mem_ctx);
		}
		progress2 = do_dead_code_local(ir); progress |= progress2; if (progress2) debug_print_ir ("After dead code local", ir, state, mem_ctx);
		progress2 = propagate_precision (ir, state->metal_target); progress |= progress2; if (progress2) debug_print_ir ("After prec propagation", ir, state, mem_ctx);
        if (!agal)
            progress2 = do_tree_grafting(ir); progress |= progress2; if (progress2) debug_print_ir ("After tree grafting", ir, state, mem_ctx);
		progress2 = do_constant_propagation(ir); progress |= progress2; if (progress2) debug_print_ir ("After const propagation", ir, state, mem_ctx);
		if (linked) {
			progress2 = do_constant_variable(ir); progress |= progress2; if (progress2) debug_print_ir ("After const variable", ir, state, mem_ctx);
		} else {
			progress2 = do_constant_variable_unlinked(ir); progress |= progress2; if (progress2) debug_print_ir ("After const variable unlinked", ir, state, mem_ctx);
		}
		progress2 = do_constant_folding(ir); progress |= progress2; if (progress2) debug_print_ir ("After const folding", ir, state, mem_ctx);
		progress2 = do_minmax_prune(ir); progress |= progress2; if (progress2) debug_print_ir ("After minmax prune", ir, state, mem_ctx);
		progress2 = do_cse(ir); progress |= progress2; if (progress2) debug_print_ir ("After CSE", ir, state, mem_ctx);
		progress2 = do_rebalance_tree(ir); progress |= progress2; if (progress2) debug_print_ir ("After rebalance tree", ir, state, mem_ctx);
		progress2 = do_algebraic(ir, state->ctx->Const.NativeIntegers, &state->ctx->Const.ShaderCompilerOptions[state->stage]); progress |= progress2; if (progress2) debug_print_ir ("After algebraic", ir, state, mem_ctx);
        if (agal)
            progress2 = lower_discard(ir); progress |= progress2; if (progress2) debug_print_ir ("After lower discard", ir, state, mem_ctx);
        progress2 = do_lower_jumps(ir); progress |= progress2; if (progress2) debug_print_ir ("After lower jumps", ir, state, mem_ctx);
        if (agal)
            progress2 = lower_if_to_cond_assign(ir); progress |= progress2; if (progress2) debug_print_ir ("After lower if", ir, state, mem_ctx);
        progress2 = do_vec_index_to_swizzle(ir); progress |= progress2; if (progress2) debug_print_ir ("After vec index to swizzle", ir, state, mem_ctx);
		progress2 = lower_vector_insert(ir, false); progress |= progress2; if (progress2) debug_print_ir ("After lower vector insert", ir, state, mem_ctx);
		progress2 = do_swizzle_swizzle(ir); progress |= progress2; if (progress2) debug_print_ir ("After swizzle swizzle", ir, state, mem_ctx);
		progress2 = do_noop_swizzle(ir); progress |= progress2; if (progress2) debug_print_ir ("After noop swizzle", ir, state, mem_ctx);
		progress2 = optimize_split_arrays(ir, linked, state->metal_target && state->stage == MESA_SHADER_FRAGMENT); progress |= progress2; if (progress2) debug_print_ir ("After split arrays", ir, state, mem_ctx);
		progress2 = optimize_redundant_jumps(ir); progress |= progress2; if (progress2) debug_print_ir ("After redundant jumps", ir, state, mem_ctx);

		// do loop stuff only when linked; otherwise causes duplicate loop induction variable
		// problems (ast-in.txt test)
		if (linked)
		{
			loop_state *ls = analyze_loop_variables(ir);
			if (ls->loop_found) {
				progress2 = set_loop_controls(ir, ls); progress |= progress2; if (progress2) debug_print_ir ("After set loop", ir, state, mem_ctx);
				progress2 = unroll_loops(ir, ls, &state->ctx->Const.ShaderCompilerOptions[state->stage]); progress |= progress2; if (progress2) debug_print_ir ("After unroll", ir, state, mem_ctx);
			}
			delete ls;
		}

        if (agal) {
            progress2 = do_agal_expression_flattening(ir, false); progress |= progress2; if (progress2) debug_print_ir("After agal expression flattening", ir, state, mem_ctx);
            progress2 = do_lower_conditionl_assigns_to_agal(ir); progress |= progress2; if (progress2) debug_print_ir("post-lowercond", ir, state, mem_ctx);
            progress2 = lower_equivalent_builtin_to_agal(ir); progress |= progress2; if (progress2) debug_print_ir("After builtin equivalence flattening", ir, state, mem_ctx);
        }

	} while (progress && passes < kMaximumPasses);

	if (!state->metal_target && !agal)
	{
		// GLSL/ES does not have saturate, so lower it
		lower_instructions(ir, SAT_TO_CLAMP);
	}
}

static void glsl_type_to_optimizer_desc(const glsl_type* type, glsl_precision prec, glslopt_shader_var* out)
{
	out->arraySize = type->array_size();

	// type; use element type when in array
	if (type->is_array())
		type = type->element_type();

	if (type->is_float())
		out->type = kGlslTypeFloat;
	else if (type->is_integer())
		out->type = kGlslTypeInt;
	else if (type->is_boolean())
		out->type = kGlslTypeBool;
	else if (type->is_sampler())
	{
		if (type->sampler_dimensionality == GLSL_SAMPLER_DIM_2D)
		{
			if (type->sampler_shadow)
				out->type = kGlslTypeTex2DShadow;
			else if (type->sampler_array)
				out->type = kGlslTypeTex2DArray;
			else
				out->type = kGlslTypeTex2D;
		}
		else if (type->sampler_dimensionality == GLSL_SAMPLER_DIM_3D)
			out->type = kGlslTypeTex3D;
		else if (type->sampler_dimensionality == GLSL_SAMPLER_DIM_CUBE)
			out->type = kGlslTypeTexCube;
		else
			out->type = kGlslTypeOther;
	}
	else
		out->type = kGlslTypeOther;

	// sizes
	out->vectorSize = type->vector_elements;
	out->matrixSize = type->matrix_columns;

	// precision
	switch (prec)
	{
		case glsl_precision_high: out->prec = kGlslPrecHigh; break;
		case glsl_precision_medium: out->prec = kGlslPrecMedium; break;
		case glsl_precision_low: out->prec = kGlslPrecLow; break;
		default: out->prec = kGlslPrecHigh; break;
	}
}

static void find_shader_variables(glslopt_shader* sh, exec_list* ir)
{
	foreach_in_list(ir_instruction, node, ir)
	{
		ir_variable* const var = node->as_variable();
		if (var == NULL)
			continue;
		if (var->data.mode == ir_var_shader_in)
		{
			if (sh->inputCount >= glslopt_shader::kMaxShaderInputs)
				continue;

			glslopt_shader_var& v = sh->inputs[sh->inputCount];
			v.name = ralloc_strdup(sh, var->name);
			glsl_type_to_optimizer_desc(var->type, (glsl_precision)var->data.precision, &v);
			v.location = var->data.explicit_location ? var->data.location : -1;
			++sh->inputCount;
		}
		if (var->data.mode == ir_var_uniform && !var->type->is_sampler())
		{
			if (sh->uniformCount >= glslopt_shader::kMaxShaderUniforms)
				continue;

			glslopt_shader_var& v = sh->uniforms[sh->uniformCount];
			v.name = ralloc_strdup(sh, var->name);
			glsl_type_to_optimizer_desc(var->type, (glsl_precision)var->data.precision, &v);
			v.location = var->data.explicit_location ? var->data.location : -1;
			++sh->uniformCount;
		}
		if (var->data.mode == ir_var_uniform && var->type->is_sampler())
		{
			if (sh->textureCount >= glslopt_shader::kMaxShaderTextures)
				continue;

			glslopt_shader_var& v = sh->textures[sh->textureCount];
			v.name = ralloc_strdup(sh, var->name);
			glsl_type_to_optimizer_desc(var->type, (glsl_precision)var->data.precision, &v);
			v.location = var->data.explicit_location ? var->data.location : -1;
			++sh->textureCount;
		}
	}
}

std::string str_replace(const std::string &str, const std::string &pattern, const std::string &dstPattern, int count = -1)
{
    std::string retStr(str);
    std::string::size_type pos = 0;
    int l_count = 0;
    if (-1 == count)
        count = retStr.size();
    while ((pos = retStr.find(pattern, pos)) != std::string::npos)
    {
        retStr.replace(pos, pattern.size(), dstPattern);
        if (++l_count >= count)
            break;
        pos += dstPattern.size();
    }
    return retStr;
}

glslopt_shader* glslopt_optimize (glslopt_ctx* ctx, glslopt_shader_type type, const char* shaderSource, unsigned options)
{
	glslopt_shader* shader = new (ctx->mem_ctx) glslopt_shader ();

	printMode = kPrintGlslVertex;
	switch (type) {
	case kGlslOptShaderVertex:
			shader->shader->Type = GL_VERTEX_SHADER;
			shader->shader->Stage = MESA_SHADER_VERTEX;
			printMode = kPrintGlslVertex;
			break;
	case kGlslOptShaderFragment:
			shader->shader->Type = GL_FRAGMENT_SHADER;
			shader->shader->Stage = MESA_SHADER_FRAGMENT;
			printMode = kPrintGlslFragment;
			break;
	}
	if (!shader->shader->Type)
	{
		shader->infoLog = ralloc_asprintf (shader, "Unknown shader type %d", (int)type);
		shader->status = false;
		return shader;
	}

	_mesa_glsl_parse_state* state = new (shader) _mesa_glsl_parse_state (&ctx->mesa_ctx, shader->shader->Stage, shader);
	if (ctx->target == kGlslTargetMetal)
		state->metal_target = true;
	state->error = 0;

	if (!(options & kGlslOptionSkipPreprocessor))
	{
		state->error = !!glcpp_preprocess (state, &shaderSource, &state->info_log, state->extensions, &ctx->mesa_ctx);
		if (state->error)
		{
			shader->status = !state->error;
			shader->infoLog = state->info_log;
			return shader;
		}
	}

    const bool debug = options & kGlslOptionDebugInfo;

	_mesa_glsl_lexer_ctor (state, shaderSource);
	_mesa_glsl_parse (state);
	_mesa_glsl_lexer_dtor (state);

	exec_list* ir = new (shader) exec_list();
	shader->shader->ir = ir;

	if (!state->error && !state->translation_unit.is_empty())
		_mesa_ast_to_hir (ir, state);

	// Un-optimized output
	if (!state->error) {
		validate_ir_tree(ir);
		if (ctx->target == kGlslTargetMetal)
			shader->rawOutput = _mesa_print_ir_metal(ir, state, ralloc_strdup(shader, ""), printMode, &shader->uniformsSize);
		else
			shader->rawOutput = _mesa_print_ir_glsl(ir, state, ralloc_strdup(shader, ""), printMode);
	}

	// Link built-in functions
	shader->shader->symbols = state->symbols;
	shader->shader->uses_builtin_functions = state->uses_builtin_functions;

	struct gl_shader* linked_shader = NULL;

	hash_table *oldnames = NULL;

	shader->optimizedOutput = NULL;
	ralloc_asprintf_append (&shader->optimizedOutput, "{\n");

	dump("pre-opt", ir, state, printMode);

    const bool toAgal = options & kGlslOptionToAgalShader;

	// Optimization passes
	const bool linked = !(options & kGlslOptionNotFullShader);
	if (!state->error && !ir->is_empty() && !(options & kGlslOptionNotFullShader))
	{
		linked_shader = link_intrastage_shaders(shader,
												&ctx->mesa_ctx,
												shader->whole_program,
												shader->whole_program->Shaders,
												shader->whole_program->NumShaders);
		if (!linked_shader)
		{
			shader->status = false;
			shader->infoLog = shader->whole_program->InfoLog;
			return shader;
		}
		ir = linked_shader->ir;

		debug_print_ir ("==== After link ====", ir, state, shader);
	}

	// Do optimization post-link
	if (!state->error && !ir->is_empty())
	{
		const bool linked = !(options & kGlslOptionNotFullShader);
		do_optimization_passes(ir, linked, state, shader, toAgal);

        if (toAgal) {
            dump("post-opt", ir, state, printMode);
            //do_tree_grafting(ir); dump("after-graft", ir, state, printMode);
            //do_agal_expression_flattening(ir, true); dump("after-flattening", ir, state, printMode);
            do_lower_arrays(ir); dump("after-lower-arrays", ir, state, printMode);
            do_tree_grafting(ir); dump("after-graft", ir, state, printMode);
            do_agal_expression_flattening(ir, true); dump("after-flattening", ir, state, printMode);
            do_algebraic(ir, state->ctx->Const.NativeIntegers, &state->ctx->Const.ShaderCompilerOptions[state->stage]); dump("after-algebraic", ir, state, printMode);
            for (int i = 0; i < 6; i++) {
                do_shorten_liveranges(ir);  dump("after-shorten-liveranges", ir, state, printMode);
            }

            do_hoist_constants(ir); dump("after-hoist-constants", ir, state, printMode);
            do_remove_casts(ir); dump("after-remove-casts", ir, state, printMode);
            // do_agal_expression_flattening(ir, true); dump("after-flattening", ir, state, printMode);

            // dump("post-opt", ir, state, printMode);
            do_swizzle_everything(ir); dump("after-swizz", ir, state, printMode);
            do_coalesce_floats(ir); dump("after-coalesce-floats", ir, state, printMode);
            do_swizzle_swizzle(ir); dump("after-swizzle-swizzle", ir, state, printMode);
            do_coalesce_temps(ir); dump("after-coalesce-temps", ir, state, printMode);
            do_unique_variables(ir); dump("after-unique-varaibles", ir, state, printMode);
            oldnames = do_remap_agalvars(ir, printMode); dump("after-remap-agalvars", ir, state, printMode);
        }
        validate_ir_tree(ir);
	}

    if (glslOptimizerVerbose)
        fflush(stderr);

	// Final optimized output
    if (!toAgal) {
        if (!state->error) {
            if (ctx->target == kGlslTargetMetal)
                shader->optimizedOutput = _mesa_print_ir_metal(ir, state, ralloc_strdup(shader, ""), printMode, &shader->uniformsSize);
            else
                shader->optimizedOutput = _mesa_print_ir_glsl(ir, state, ralloc_strdup(shader, ""), printMode);
        }
    }
    else {
        const char *agalasmout = strlen(state->info_log) > 0 ? "" : _mesa_print_ir_agal(ir, state, ralloc_strdup(shader, ""), printMode);

        const char *infoLog = state->info_log;
        std::string sanitisedInfoLog;
        while (infoLog && *infoLog)
        {
            switch (*infoLog) {
                case 10:
                case 13:
                    sanitisedInfoLog += "\\n";
                    break;
                default:
                    sanitisedInfoLog += infoLog[0];
            }
            infoLog++;
        }
        ralloc_asprintf_append(&shader->optimizedOutput, "\"info\":\"%s\"", sanitisedInfoLog.c_str());

        // Final optimized output
        if (strlen(state->info_log) > 0) {
            ralloc_asprintf_append(&shader->optimizedOutput, "\n}\n");
        }
        else {
            ralloc_asprintf_append(&shader->optimizedOutput, ",\n");
            //shader->optimizedOutput = NULL;

            if (debug) {
                ralloc_asprintf_append(&shader->optimizedOutput, "\"glsl-src\":\"%s\",\n", str_replace(shaderSource, "\n", "\\n").c_str());
                ralloc_asprintf_append(&shader->optimizedOutput, "\"glsl-raw\":\"%s\",\n", str_replace(shader->rawOutput, "\n", "\\n").c_str());
                const char *glslout = _mesa_print_ir_glsl(ir, state, ralloc_strdup(ctx->mem_ctx, ""), printMode);
                ralloc_asprintf_append(&shader->optimizedOutput, "\"glsl-final\":\"%s\",\n", str_replace(glslout, "\n", "\\n").c_str());
            }

            if (NULL != oldnames) {
                ralloc_asprintf_append(&shader->optimizedOutput, "\"varnames\" : \n{\n");

                emitComma = false;
                hash_table_call_foreach(oldnames, print_agal_var_mapping, shader);
                ralloc_asprintf_append(&shader->optimizedOutput, "},\n");
            }

            emitComma = false;
            ralloc_asprintf_append(&shader->optimizedOutput, "\"consts\" : \n{\n");
            do_print_constants(ir, shader);
            ralloc_asprintf_append(&shader->optimizedOutput, "},\n");

            emitComma = false;
            ralloc_asprintf_append(&shader->optimizedOutput, "\"types\" : \n{\n");
            do_print_types(ir, shader);
            ralloc_asprintf_append(&shader->optimizedOutput, "},\n");

            emitComma = false;
            ralloc_asprintf_append(&shader->optimizedOutput, "\"storage\" : \n{\n");
            do_print_storage(ir, shader);
            ralloc_asprintf_append(&shader->optimizedOutput, "},\n");

            if (glslOptimizerVerbose)
                fprintf(stderr, "agal:\n%s", agalasmout);
            std::string sanitisedAGAL;
            const char *asmsrc = agalasmout;
            while (*asmsrc)
            {
                switch (*asmsrc) {
                    case 10:
                    case 13:
                        sanitisedAGAL += "\\n";
                        break;
                    default:
                        sanitisedAGAL += asmsrc[0];
                }
                asmsrc++;
            }
            ralloc_asprintf_append(&shader->optimizedOutput, "\"agalasm\":\"%s\"\n}\n", sanitisedAGAL.c_str());

            if (false && glslOptimizerVerbose) {
                char *agalout;
                size_t agalsz = 0;
                AGAL::Assemble(agalasmout, printMode == kPrintGlslFragment ? AGAL::shadertype_fragment : AGAL::shadertype_vertex, &agalout, &agalsz);

                AGAL::Graph *depgraph = AGAL::CreateDependencyGraph(agalout, agalsz, 0);
                fprintf(stderr, "digraph agaldepgraph {\n");
                for(int i=0; i<depgraph->edges.length; i++) {
                	fprintf(stderr, "%d -> %d\n", depgraph->edges[i].srcidx, depgraph->edges[i].dstidx);
                }
                fprintf(stderr, "}\n");

                FlashString disasmout;
                if (agalout)
                    AGAL::Disassemble(agalout, agalsz, &disasmout);
                fprintf(stderr, "//--- Disasm ---\n");
                fprintf(stderr, "%s", disasmout.CStr());
                fprintf(stderr, "//--- Disasm ---\n");
            }
        }
    }

	shader->status = !state->error;
	shader->infoLog = state->info_log;

	find_shader_variables (shader, ir);
	if (!state->error)
		calculate_shader_stats (ir, &shader->statsMath, &shader->statsTex, &shader->statsFlow);

	ralloc_free (ir);
	ralloc_free (state);

	if (linked_shader)
		ralloc_free(linked_shader);

	return shader;
}

void glslopt_shader_delete (glslopt_shader* shader)
{
	delete shader;
}

bool glslopt_get_status (glslopt_shader* shader)
{
	return shader->status;
}

const char* glslopt_get_output (glslopt_shader* shader)
{
	return shader->optimizedOutput;
}

const char* glslopt_get_raw_output (glslopt_shader* shader)
{
	return shader->rawOutput;
}

const char* glslopt_get_log (glslopt_shader* shader)
{
	return shader->infoLog;
}

int glslopt_shader_get_input_count (glslopt_shader* shader)
{
	return shader->inputCount;
}

int glslopt_shader_get_uniform_count (glslopt_shader* shader)
{
	return shader->uniformCount;
}

int glslopt_shader_get_uniform_total_size (glslopt_shader* shader)
{
	return shader->uniformsSize;
}

int glslopt_shader_get_texture_count (glslopt_shader* shader)
{
	return shader->textureCount;
}

void glslopt_shader_get_input_desc (glslopt_shader* shader, int index, const char** outName, glslopt_basic_type* outType, glslopt_precision* outPrec, int* outVecSize, int* outMatSize, int* outArraySize, int* outLocation)
{
	const glslopt_shader_var& v = shader->inputs[index];
	*outName = v.name;
	*outType = v.type;
	*outPrec = v.prec;
	*outVecSize = v.vectorSize;
	*outMatSize = v.matrixSize;
	*outArraySize = v.arraySize;
	*outLocation = v.location;
}

void glslopt_shader_get_uniform_desc (glslopt_shader* shader, int index, const char** outName, glslopt_basic_type* outType, glslopt_precision* outPrec, int* outVecSize, int* outMatSize, int* outArraySize, int* outLocation)
{
	const glslopt_shader_var& v = shader->uniforms[index];
	*outName = v.name;
	*outType = v.type;
	*outPrec = v.prec;
	*outVecSize = v.vectorSize;
	*outMatSize = v.matrixSize;
	*outArraySize = v.arraySize;
	*outLocation = v.location;
}

void glslopt_shader_get_texture_desc (glslopt_shader* shader, int index, const char** outName, glslopt_basic_type* outType, glslopt_precision* outPrec, int* outVecSize, int* outMatSize, int* outArraySize, int* outLocation)
{
	const glslopt_shader_var& v = shader->textures[index];
	*outName = v.name;
	*outType = v.type;
	*outPrec = v.prec;
	*outVecSize = v.vectorSize;
	*outMatSize = v.matrixSize;
	*outArraySize = v.arraySize;
	*outLocation = v.location;
}

void glslopt_shader_get_stats (glslopt_shader* shader, int* approxMath, int* approxTex, int* approxFlow)
{
	*approxMath = shader->statsMath;
	*approxTex = shader->statsTex;
	*approxFlow = shader->statsFlow;
}
