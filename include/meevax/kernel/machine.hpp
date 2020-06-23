#ifndef INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
#define INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/de_brujin_index.hpp>
#include <meevax/kernel/identifier.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/syntax.hpp>

namespace meevax::kernel
{
  template <typename SK>
  class machine // Simple SECD machine.
  {
    friend SK;

    machine()
    {}

    Import(SK, debug);
    Import(SK, evaluate);
    Import(SK, indent);
    Import(SK, intern);
    Import(SK, rename);
    Import(SK, syntactic_environment);
    Import_Const(SK, current_debug_port);
    Import_Const(SK, current_error_port);
    Import_Const(SK, current_output_port);
    Import_Const(SK, header);
    Import_Const(SK, shift);
    Import_Const(SK, tracing);
    Import_Const(SK, write_to);

  protected:
    object s, // Stack (holding intermediate results and return address)
           e, // Environment (giving values to symbols)
           c, // Control (instructions yet to be executed)
           d; // Dump (S.E.C)

  public:
    template <typename... Ts>
    decltype(auto) define(const object& variable, Ts&&... expression)
    {
      push(
        syntactic_environment(),
        list( // TODO => cons
          variable,
          Perfect_Forward(expression)...));

      write_to(current_debug_port(),
        header("define"),
        caar(syntactic_environment()),
        console::faint, " binds ", console::reset,
        cadar(syntactic_environment()),
        "\n");

      return unspecified;
    }

    const object& glocal_environment(const object& e)
    {
      for (const auto& frame : e)
      {
        if (frame and car(frame) and car(frame).is<SK>())
        {
          return cdar(frame);
        }
      }

      return syntactic_environment();
    }

    /* ------------------------------------------------------------------------
    *
    * <expression> = <identifier>
    *              | <literal>
    *              | <procedure call>
    *              | <lambda expression>
    *              | <conditional>
    *              | <assignment>
    *              | <derived expression>
    *
    *----------------------------------------------------------------------- */
    object compile(
      const object& expression,
      const object& syntactic_environment,
      const object& frames = unit,
      const object& continuation = list(make<instruction>(mnemonic::STOP)),
      const compilation_context in_a = as_is)
    {
      if (not expression)
      {
        return cons(make<instruction>(mnemonic::LOAD_CONSTANT), unit, continuation);
      }
      else if (not expression.is<pair>()) // is <identifier>
      {
        if (is_identifier(expression))
        {
          if (de_bruijn_index index {expression, frames}; index)
          {
            if (index.is_variadic())
            {
              debug(expression, console::faint, " ; is a <variadic bound variable> references ", console::reset, index);

              return
                cons(
                  make<instruction>(mnemonic::LOAD_VARIADIC), index,
                  continuation);
            }
            else
            {
              debug(expression, console::faint, " ; is a <bound variable> references ", console::reset, index);

              return
                cons(
                  make<instruction>(mnemonic::LOAD_LOCAL), index,
                  continuation);
            }
          }
          else if (expression.is<syntactic_closure>())
          {
            debug(expression, console::faint, " ; is <alias>");

            return
              cons(
                make<instruction>(mnemonic::STRIP), expression,
                continuation);
          }
          else
          {
            debug(expression, console::faint, " ; is a <glocal variable>");

            return
              cons(
                make<instruction>(mnemonic::LOAD_GLOBAL), expression,
                continuation);
          }
        }
        else // is <self-evaluating>
        {
          debug(expression, console::faint, " ; is <self-evaluating>");

          return
            cons(
              make<instruction>(mnemonic::LOAD_CONSTANT), expression,
              continuation);
        }
      }
      else // is (application . arguments)
      {
        if (const object applicant { lookup(car(expression), syntactic_environment) };
            not null(applicant) and not de_bruijn_index(car(expression), frames))
        {
          if (applicant.is<syntax>())
          {
            debug(
              console::magenta, "(",
              console::reset, car(expression),
              console::faint, " ; is <primitive expression>");

            indent() >> shift();

            auto result {
              std::invoke(applicant.as<syntax>(),
                cdr(expression), syntactic_environment, frames, continuation, in_a)
            };

            debug(console::magenta, ")");

            indent() << shift();

            return result;
          }
          else if (applicant.is<SK>())
          {
            debug(console::magenta, "(", console::reset, car(expression), console::faint, " ; is <macro application>");

            const auto expanded {
              // applicant.as<SK>().expand(cons(applicant, cdr(expression)))
              applicant.as<SK>().expand(applicant, expression)
            };

            debug(expanded);

            return compile(expanded, syntactic_environment, frames, continuation);
          }
        }

        debug(console::magenta, "(", console::reset, console::faint, " ; is <procedure call>");
        indent() >> shift();

        auto result
        {
          operand(
            cdr(expression),
            syntactic_environment,
            frames,
            compile(
              car(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(in_a.tail_expression ? mnemonic::TAIL_CALL : mnemonic::CALL),
                continuation)))
        };

        debug(console::magenta, ")");
        indent() << shift();

        return result;
      }
    }

    void disassemble(const object& c, std::size_t depth = 1)
    {
      assert(0 < depth);

      for (homoiconic_iterator iter {c}; iter; ++iter)
      {
        write_to(current_debug_port(), "; ");

        if (iter == c)
        {
          write_to(current_debug_port(),
            std::string(4 * (depth - 1), ' '), console::magenta, "(   ");
        }
        else
        {
          write_to(current_debug_port(), std::string(4 * depth, ' '));
        }

        switch ((*iter).as<instruction>().code)
        {
        case mnemonic::CALL:
        case mnemonic::CONS:
        case mnemonic::DROP:
        case mnemonic::JOIN:
        case mnemonic::TAIL_CALL:
          write_to(current_debug_port(), *iter, "\n");
          break;

        case mnemonic::RETURN:
        case mnemonic::STOP:
          write_to(current_debug_port(), *iter, console::magenta, "\t)\n");
          break;

        case mnemonic::DEFINE:
        case mnemonic::FORK:
        case mnemonic::LOAD_CONSTANT:
        case mnemonic::LOAD_GLOBAL:
        case mnemonic::LOAD_LOCAL:
        case mnemonic::LOAD_VARIADIC:
        case mnemonic::STORE_GLOBAL:
        case mnemonic::STORE_LOCAL:
        case mnemonic::STORE_VARIADIC:
        case mnemonic::STRIP:
          // NOTE: evaluation order of function argument is undefined (C++).
          write_to(current_debug_port(), *iter);
          write_to(current_debug_port(), " ", *++iter, "\n");
          break;

        case mnemonic::LOAD_CLOSURE:
        case mnemonic::LOAD_CONTINUATION:
          write_to(current_debug_port(), *iter, "\n");
          disassemble(*++iter, depth + 1);
          break;


        case mnemonic::SELECT:
        case mnemonic::TAIL_SELECT:
          write_to(current_debug_port(), *iter, "\n");
          disassemble(*++iter, depth + 1);
          disassemble(*++iter, depth + 1);
          break;

        default:
          assert(false);
        }
      }
    }

    object execute()
    {
    dispatch:
      if (tracing())
      {
        std::cerr << "; trace s\t; " <<  s << std::endl;
        std::cerr << ";       e\t; " <<  e << std::endl;
        std::cerr << ";       c\t; " <<  c << std::endl;
        std::cerr << ";       d\t; " <<  d << std::endl;
        std::cerr << std::endl;
      }

      switch (car(c).template as<instruction>().code)
      {
      case mnemonic::LOAD_LOCAL: /* ============================================
        *
        *              S  E (LOAD-LOCAL (i . j) . C) D
        * => (result . S) E                       C  D
        *
        * where result = (list-ref (list-ref E i) j)
        *
        * =================================================================== */
        push(
          s,
          list_reference(
            list_reference(
              e,
              static_cast<int>(caadr(c).template as<real>())),
            static_cast<int>(cdadr(c).template as<real>())));
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_VARIADIC: /* =========================================
        *
        *              S  E (LOAD-VARIADIC (i . j) . C) D
        * => (result . S) E                          C  D
        *
        * where result = (list-tail (list-ref E i) j)
        *
        * =================================================================== */
        push(
          s,
          list_tail(
            list_reference(
              e,
              static_cast<int>(caadr(c).template as<real>())),
            static_cast<int>(cdadr(c).template as<real>())));
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_CONSTANT: /* =========================================
        *
        *                S  E (LOAD-CONSTANT constant . C) D
        * => (constant . S) E                           C  D
        *
        * =================================================================== */
        // push(s, strip(cadr(c)));
        push(s, cadr(c));
        pop<2>(c);
        goto dispatch;

      // case mnemonic::LOAD_SYNTAX: /* ===========================================
      //   *
      //   *                S  E (LOAD-CONSTANT syntax . C) D
      //   * => (constant . S) E                         C  D
      //   *
      //   * =================================================================== */
      //   push(s, cadr(c));
      //   pop<2>(c);
      //   goto dispatch;

      case mnemonic::LOAD_GLOBAL: /* ===========================================
        *
        *              S  E (LOAD-GLOBAL symbol . C) D
        * => (object . S) E                       C  D
        *
        * =================================================================== */
        if (const object binding { assq(cadr(c), glocal_environment(e)) }; not binding.eqv(f))
        {
          push(s, cadr(binding));
        }
        else // UNBOUND
        {
          push(s, rename(cadr(c)));
        }
        pop<2>(c);
        goto dispatch;

      case mnemonic::STRIP: /* =================================================
        *
        *            S  E (STRIP syntactic-closure . C) D
        * => (form . S) E                            C  D
        *
        * =================================================================== */
        push(s, cadr(c).template as<syntactic_closure>().strip());
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_CLOSURE: /* ==========================================
        *
        *               S  E (LOAD-CLOSURE body . C) D
        * => (closure . S) E                      C  D
        *
        * =================================================================== */
        push(s, make<closure>(cadr(c), e));
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_CONTINUATION: /* =====================================
        *
        *                      S  E (LDK cc . C) D
        * => ((continuation) . S) E           C  D
        *
        * =================================================================== */
        push(s, list(make<continuation>(s, cons(e, cadr(c), d))));
        pop<2>(c);
        goto dispatch;

      case mnemonic::FORK: /* ==================================================
        *
        *                  S  E (FORK csc . C) D
        * => (subprogram . S) E             C  D
        *
        * =================================================================== */
        push(s,
          make<SK>(
            cons(s, e, cadr(c), d), // current-syntactic-continuation
            syntactic_environment()));
        pop<2>(c);
        goto dispatch;

      case mnemonic::SELECT: /* ================================================
        *
        *    (test . S) E (SELECT consequent alternate . C)  D
        * =>         S  E         selection             (C . D)
        *
        * where selection = (if test consequent alternate)
        *
        * =================================================================== */
        push(d, cdddr(c));
        [[fallthrough]];

      case mnemonic::TAIL_SELECT: /* ===========================================
        *
        *    (test . S) E (SELECT consequent alternate . C)  D
        * =>         S  E         selection                  D
        *
        * where selection = (if test consequent alternate)
        *
        * =================================================================== */
        c = not car(s).equivalent_to(f) ? cadr(c) : caddr(c);
        pop<1>(s);
        goto dispatch;

      case mnemonic::JOIN: /* ==================================================
        *
        *      S E (JOIN) (C . D)
        *   => S E         C   D
        *
        * =================================================================== */
        c = car(d);
        pop<1>(d);
        goto dispatch;

      case mnemonic::DEFINE: /* ================================================
        *
        *          (object . S) E (DEFINE identifier . C) D
        *   => (identifier . S) E                      C  D
        *
        * =================================================================== */
        if (0 < static_cast<SK&>(*this).generation)
        {
          std::cerr << "; define\t; redefinition of " << cadr(c) << " is ignored" << std::endl;
        }
        else
        {
          car(s) = define(cadr(c), car(s));
        }
        pop<2>(c);
        goto dispatch;

      case mnemonic::CALL:
        if (const object callee {car(s)}; not callee)
        {
          static const error e {"unit is not appliciable"};
          throw e;
        }
        else if (callee.is<closure>()) // (closure operands . S) E (CALL . C) D
        {
          push(d, cddr(s), e, cdr(c));
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure operands . S) E (CALL . C) D => (result . S) E C D
        {
          s = cons(std::invoke(callee.as<procedure>(), cadr(s)), cddr(s));
          pop<1>(c);
        }
        // else if (callee.is<SK>()) // TODO REMOVE
        // {
        //   s = cons(
        //         callee.as<SK>().evaluate(
        //           cadr(s)),
        //         cddr(s));
        //   pop<1>(c);
        // }
        else if (callee.is<continuation>()) // (continuation operands . S) E (CALL . C) D
        {
          s = cons(
                caadr(s),
                car(callee));
          e =  cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
        }
        else
        {
          throw evaluation_error {callee, " is not applicable"};
        }
        goto dispatch;

      case mnemonic::TAIL_CALL:
        if (object callee {car(s)}; not callee)
        {
          throw evaluation_error {"unit is not appliciable"};
        }
        else if (callee.is<closure>()) // (closure operands . S) E (CALL . C) D
        {
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure operands . S) E (CALL . C) D => (result . S) E C D
        {
          s = cons(std::invoke(callee.as<procedure>(), cadr(s)), cddr(s));
          pop<1>(c);
        }
        // else if (callee.is<SK>()) // TODO REMOVE
        // {
        //   s = cons(
        //         callee.as<SK>().evaluate(
        //           cadr(s)),
        //         cddr(s));
        //   pop<1>(c);
        // }
        else if (callee.is<continuation>()) // (continuation operands . S) E (CALL . C) D
        {
          s = cons(
                caadr(s),
                car(callee));
          e =  cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
        }
        else
        {
          throw evaluation_error {callee, " is not applicable"};
        }
        goto dispatch;

      case mnemonic::RETURN: /* ================================================
        *
        *      (result . S)  E (RETURN . C) (S' E' C' . D)
        *   => (result . S') E'          C'             D
        *
        * =================================================================== */
        s = cons(car(s), pop(d));
        e = pop(d);
        c = pop(d);
        goto dispatch;

      case mnemonic::CONS: /* ==================================================
        *
        *      ( X   Y  . S) E (CONS . C) D
        *   => ((X . Y) . S) E         C  D
        *
        * =================================================================== */
        s = cons(cons(car(s), cadr(s)), cddr(s));
        pop<1>(c);
        goto dispatch;

      case mnemonic::DROP: /* ==================================================
        *
        *     (result . S) E (DROP . C) D
        *   =>          S  E         C  D
        *
        * =================================================================== */
        pop<1>(s);
        pop<1>(c);
        goto dispatch;

      case mnemonic::STORE_GLOBAL: /* ==========================================
        *
        *      (value . S) E (STORE-GLOBAL identifier . C) D
        *   => (value . S) E                            C  D
        *
        * TODO
        *   (1) There is no need to make copy if right hand side is unique.
        *   (2) There is no matter overwrite if left hand side is unique.
        *   (3) Should set with weak reference if right hand side is newer.
        *
        * =================================================================== */
        if (const object pare { assq(cadr(c), glocal_environment(e)) }; not pare.eqv(f))
        {
          if (const auto value {cadr(pare)}; not value or not car(s))
          {
            cadr(pare) = car(s);
          }
          else if (value.is<SK>() or value.is<syntax>())
          {
            /* ---- From R7RS 5.3.1. Top level definitions ---------------------
            *
            * However, if <variable> is not bound, or is a syntactic keyword,
            * then the definition will bind <variable> to a new location before
            * performing the assignment, whereas it would be an error to perform
            * a set! on an unbound variable.
            *
            *---------------------------------------------------------------- */
            define(cadr(c), car(s));
          }
          else
          {
            std::atomic_store(&cadr(pare), car(s).copy());
          }
        }
        else // UNBOUND
        {
          // TODO IF TOPLELVEL, SET ON UNBOUND VARIABLE => DEFINE

          throw evaluation_error {
            "it would be an error to perform a set! on an unbound variable (from R7RS 5.3.1. Top level definitions)"
          };
        }
        // car(s) = unspecified;
        pop<2>(c);
        goto dispatch;

      case mnemonic::STORE_LOCAL: /* ===========================================
        *
        *      (value . S) E (STORE-LOCAL (i . j) . C) D
        *   => (value . S) E                        C  D
        *
        * =================================================================== */
        std::atomic_store(&
          car(
            list_tail(
              list_reference(
                e,
                static_cast<int>(caadr(c).template as<real>())),
              static_cast<int>(cdadr(c).template as<real>()))),
          car(s));
        pop<2>(c);
        goto dispatch;

      case mnemonic::STORE_VARIADIC:
        std::atomic_store(&
          cdr(
            list_tail(
              list_reference(
                e,
                static_cast<int>(caadr(c).template as<real>())),
              static_cast<int>(cdadr(c).template as<real>() - 1))),
          car(s));
        pop<2>(c);
        goto dispatch;

      default: // ERROR
      case mnemonic::STOP: /* ==================================================
        *
        *    (result . S) E (STOP . C) D
        * =>           S  E         C  D
        *
        * =================================================================== */
        pop<1>(c);
        return pop(s); // return car(s);
      }
    }

  protected: // Primitive Expression Types
    #define DEFINE_PRIMITIVE_EXPRESSION(NAME)                                  \
    const object NAME(                                                         \
      [[maybe_unused]] const object& expression,                               \
      [[maybe_unused]] const object& syntactic_environment,                    \
      [[maybe_unused]] const object& frames,                                   \
      [[maybe_unused]] const object& continuation,                             \
      [[maybe_unused]] const compilation_context in_a = as_is)

    /* ==== Quotation =========================================================
    *
    * <quotation> = (quote <datum>)
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(quotation)
    {
      debug(car(expression), console::faint, " ; is <datum>");

      return
        cons(
          make<instruction>(mnemonic::LOAD_CONSTANT), car(expression),
          continuation);
    }

    /* ==== Sequence ==========================================================
    *
    * <sequence> = <command>* <expression>
    *
    * <command> = <expression>
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(sequence)
    {
      if (in_a.program_declaration)
      {
        if (not cdr(expression))
        {
          return
            compile(
              car(expression),
              syntactic_environment,
              frames,
              continuation,
              as_program_declaration);
        }
        else
        {
          return
            compile(
              car(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::DROP),
                sequence(
                  cdr(expression),
                  syntactic_environment,
                  frames,
                  continuation,
                  as_program_declaration)),
              as_program_declaration);
        }
      }
      else
      {
        if (not cdr(expression)) // is tail sequence
        {
          return
            compile(
              car(expression),
              syntactic_environment,
              frames,
              continuation);
        }
        else
        {
          return
            compile(
              car(expression), // head expression
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::DROP), // pop result of head expression
                sequence(
                  cdr(expression), // rest expressions
                  syntactic_environment,
                  frames,
                  continuation)));
        }
      }
    }

    /* ==== Definition ========================================================
    *
    * <definition> = (define <identifier> <expression>)
    *
    * TODO MOVE INTO SYNTACTIC_CONTINUATION
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(definition)
    {
      if (not frames or in_a.program_declaration)
      {
        debug(car(expression), console::faint, " ; is <variable>");

        // const auto definition {compile(
        //   cdr(expression) ? cadr(expression) : unspecified,
        //   syntactic_environment,
        //   frames,
        //   list(
        //     make<instruction>(mnemonic::DEFINE), car(expression),
        //     make<instruction>(mnemonic::STOP))
        // )};
        //
        // object result {unit};
        //
        // if (in_a.program_declaration)
        // {
        //   c = definition;
        //
        //   // std::cerr << ";\t\t; s = " << s << std::endl;
        //   // std::cerr << ";\t\t; e = " << e << std::endl;
        //   // std::cerr << ";\t\t; c = " << c << std::endl;
        //   // std::cerr << ";\t\t; d = " << d << std::endl;
        //
        //   result = execute();
        // }
        // else
        // {
        //   result = execute_interrupt(definition);
        // }
        //
        // return
        //   cons(
        //     make<instruction>(mnemonic::LOAD_CONSTANT), result,
        //     continuation);

        return
          compile(
            cdr(expression) ? cadr(expression) : unspecified,
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::DEFINE), car(expression),
              continuation));
      }
      else
      {
        throw syntax_error_about_internal_define {
          "definition cannot appear in this context"
        };
        // throw
        //   compile(
        //     cdr(expression) ? cadr(expression) : undefined,
        //     syntactic_environment,
        //     frames,
        //     cons(
        //       make<instruction>(mnemonic::DEFINE), car(expression),
        //       continuation));
      }
    }

    /* ==== Lambda Body ========================================================
     *
     * <body> = <definition>* <sequence>
     *
     * ====================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(body)
    {
      const auto flag { in_a.program_declaration ? as_program_declaration : as_is };

      auto is_definition = [&](const auto& form)
      {
        write_to(current_output_port(intern("internal-definitions")),
          "; CHECKING FORM ", form, "\n");

        try
        {
          compile(form, syntactic_environment, frames, continuation, flag);
          return false;
        }
        catch (syntax_error_about_internal_define)
        {
          return true;
        }
      };

      auto sweep = [&](const auto& form)
      {
        auto binding_specs { unit };

        for (auto iter { std::begin(form) }; iter != std::end(form); ++iter)
        {
          if (is_definition(*iter))
          {
            binding_specs = cons(cdr(*iter), binding_specs);
          }
          else
          {
            return std::make_pair(reverse(binding_specs), iter);
          }
        }

        return std::make_pair(reverse(binding_specs), std::end(form));
      };

      auto letrec = [&](const auto& binding_specs, const auto& tail_body)
      {
        std::cout << "\n"
                  << "; compiler\t; letrec\n"
                  << ";\t\t; binding-specs = " << binding_specs << "\n"
                  << ";\t\t; tail-body = " << tail_body << std::endl;

        const object variables { map(car, binding_specs) };
        std::cout << ";\t\t; variables = " << variables << std::endl;

        const object inits { make_list(length(variables), undefined) };
        std::cout << ";\t\t; inits = " << inits << std::endl;

        const object head_body {
          map(
            [this](auto&& each)
            {
              return cons(intern("set!"), each); // XXX NOT HYGIENIC!!!
            },
            binding_specs)
        };
        std::cout << ";\t\t; head_body = " << head_body << std::endl;

        const object result {
          cons(
            cons(
              intern("lambda"), // XXX NOT HYGIENIC!!!
              variables,
              append(head_body, tail_body)),
            inits)
        };

        std::cout << "\t\t; result = " << result << std::endl;

        return result;
      };

      if (not cdr(expression)) // is tail-sequence
      {
        return
          compile(
            car(expression), syntactic_environment, frames, continuation,
            in_a.program_declaration ? as_tail_expression_of_program_declaration
                                     : as_tail_expression);
      }
      else if (const auto [binding_specs, tail_body] { sweep(expression) }; binding_specs)
      {
        return
          compile(
            letrec(binding_specs, tail_body),
            syntactic_environment,
            frames,
            continuation,
            flag);
      }
      else
      {
        return
          compile(
            car(expression), syntactic_environment, frames,
            cons(
              make<instruction>(mnemonic::DROP),
              sequence(
                cdr(expression), syntactic_environment, frames, continuation, flag)),
            flag);
      }
    }
    // DEFINE_PRIMITIVE_EXPRESSION(body)
    // {
    //   /* ----------------------------------------------------------------------
    //   *
    //   * The expression may have following form.
    //   *
    //   * (lambda (...)
    //   *   <definition or command>  ;= <car expression>
    //   *   <definition or sequence> ;= <cdr expression>
    //   *   )
    //   *
    //   *---------------------------------------------------------------------- */
    //   if (not cdr(expression)) // is tail sequence
    //   {
    //     /* --------------------------------------------------------------------
    //     *
    //     * The expression may have following form.
    //     * If definition appears in <car expression>, it is an syntax error.
    //     *
    //     * (lambda (...)
    //     *   <expression> ;= <car expression>
    //     *   )
    //     *
    //     *-------------------------------------------------------------------- */
    //     if (in_a.program_declaration)
    //     {
    //       return
    //         compile(
    //           car(expression),
    //           syntactic_environment,
    //           frames,
    //           continuation,
    //           as_tail_expression_of_program_declaration);
    //     }
    //     else
    //     {
    //       return
    //         compile(
    //           car(expression),
    //           syntactic_environment,
    //           frames,
    //           continuation,
    //           as_tail_expression);
    //     }
    //   }
    //   else if (not car(expression))
    //   {
    //     /* --------------------------------------------------------------------
    //     *
    //     * The expression may have following form.
    //     * If definition appears in <cdr expression>, it is an syntax error.
    //     *
    //     * (lambda (...)
    //     *   ()         ;= <car expression>
    //     *   <sequence> ;= <cdr expression>)
    //     *
    //     *-------------------------------------------------------------------- */
    //     if (in_a.program_declaration)
    //     {
    //       return
    //         sequence(
    //           cdr(expression),
    //           syntactic_environment,
    //           frames,
    //           continuation,
    //           as_program_declaration);
    //     }
    //     else
    //     {
    //       return
    //         sequence(
    //           cdr(expression),
    //           syntactic_environment,
    //           frames,
    //           continuation);
    //     }
    //   }
    //   // XXX UGLY CODE!!!
    //   else if (    car(expression)
    //            and car(expression).is<pair>() // is application
    //            and caar(expression) // the operator is not illegal
    //            and caar(expression).template is<symbol>() // the operator is variable reference
    //            and not de_bruijn_index(caar(expression), frames) // the operator is not local binding
    //            and not assq(caar(expression), syntactic_environment).eqv(f)
    //            and cadr(assq(caar(expression), syntactic_environment)) // binding is not null
    //            and cadr(assq(caar(expression), syntactic_environment)).is<syntax>()
    //            and cadr(assq(caar(expression), syntactic_environment)).as<syntax>().name == "define")
    //   {
    //     /* --------------------------------------------------------------------
    //     *
    //     * The expression may have following form.
    //     * If definition appears in <car expression> or <cdr expression>, it is
    //     * an syntax error.
    //     *
    //     * (lambda (...)
    //     *   (define <variable> <initialization>) ;= <car expression>
    //     *   <sequence> ;= <cdr expression>)
    //     *
    //     *-------------------------------------------------------------------- */
    //     // std::cerr << "; letrec*\t; <expression> := " << expression << std::endl;
    //
    //     // <bindings> := ( (<variable> <initialization>) ...)
    //     object bindings {list(
    //       cdar(expression) // (<variable> <initialization>)
    //     )};
    //
    //     // <body> of letrec* := <sequence>+
    //     object body {};
    //
    //     /* --------------------------------------------------------------------
    //     *
    //     * Collect <definition>s from <cdr expression>.
    //     * It is guaranteed that <cdr expression> is not unit from first
    //     * conditional of this member function.
    //     *
    //     * The <cdr expression> may have following form.
    //     *
    //     * ( <expression 1>
    //     *   <expression 2>
    //     *   ...
    //     *   <expression N> )
    //     *
    //     *-------------------------------------------------------------------- */
    //     for (homoiconic_iterator each {cdr(expression)}; each; ++each)
    //     {
    //       if (not car(each) or // unit (TODO? syntax-error)
    //           not car(each).is<pair>() or // <identifier or literal>
    //           caar(each) != intern("define")) // XXX THIS IS NOT HYGIENIC
    //       {
    //         body = each;
    //
    //         // std::cerr << "; letrec*\t; <bindings> := " << bindings << std::endl;
    //         //
    //         // std::cerr << "; letrec*\t; <body> := " << body << std::endl;
    //       }
    //       else
    //       {
    //         bindings = append(bindings, list(cdar(each)));
    //       }
    //     }
    //
    //     const object formals {map(car, bindings)};
    //     // std::cerr << "; letrec*\t; <formals> := " << formals << std::endl;
    //
    //     const object operands {make_list(length(formals), undefined)};
    //     // std::cerr << "; letrec*\t; <operands> := " << operands << std::endl;
    //
    //     const object assignments {map(
    //       [this](auto&& each)
    //       {
    //         return intern("set!") | each;
    //       },
    //       bindings
    //     )};
    //     // std::cerr << "; letrec*\t; <assignments> := " << assignments << std::endl;
    //
    //     const object result {
    //       cons(
    //         cons(
    //           intern("lambda"),
    //           formals,
    //           append(assignments, body)),
    //         operands)
    //     };
    //     // std::cerr << "; letrec*\t; result := " << result << std::endl;
    //
    //     return
    //       compile(
    //         result,
    //         syntactic_environment,
    //         frames,
    //         continuation);
    //   }
    //   else
    //   {
    //     /* --------------------------------------------------------------------
    //     *
    //     * The expression may have following form.
    //     *
    //     * (lambda (...)
    //     *   <non-definition expression> ;= <car expression>
    //     *   <sequence>                  ;= <cdr expression>)
    //     *
    //     *-------------------------------------------------------------------- */
    //     if (in_a.program_declaration)
    //     {
    //       return
    //         compile(
    //           car(expression),
    //           syntactic_environment,
    //           frames,
    //           cons(
    //             make<instruction>(mnemonic::DROP),
    //             sequence(
    //               cdr(expression),
    //               syntactic_environment,
    //               frames,
    //               continuation,
    //               as_program_declaration)),
    //           as_program_declaration);
    //     }
    //     else
    //     {
    //       return
    //         compile(
    //           car(expression), // <non-definition expression>
    //           syntactic_environment,
    //           frames,
    //           cons(
    //             make<instruction>(mnemonic::DROP), // remove result of expression
    //             sequence(
    //               cdr(expression),
    //               syntactic_environment,
    //               frames,
    //               continuation)));
    //     }
    //   }
    // }

    /* ==== Operand ===========================================================
    *
    * <operand> = <expression>
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(operand)
    {
      if (expression and expression.is<pair>())
      {
        return
          operand(
            cdr(expression),
            syntactic_environment,
            frames,
            compile(
              car(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::CONS),
                continuation)));
      }
      else
      {
        return
          compile(
            expression,
            syntactic_environment,
            frames,
            continuation);
      }
    }

    /* ==== Conditional =======================================================
    *
    * <conditional> = (if <test> <consequent> <alternate>)
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(conditional)
    {
      debug(car(expression), console::faint, " ; is <test>");

      if (in_a.tail_expression)
      {
        const auto consequent {
          compile(
            cadr(expression),
            syntactic_environment,
            frames,
            list(
              make<instruction>(mnemonic::RETURN)),
            as_tail_expression)
        };

        const auto alternate {
          cddr(expression)
            ? compile(
                caddr(expression),
                syntactic_environment,
                frames,
                list(
                  make<instruction>(mnemonic::RETURN)),
                as_tail_expression)
            : list(
                make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                make<instruction>(mnemonic::RETURN))
        };

        return
          compile(
            car(expression), // <test>
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::TAIL_SELECT),
              consequent,
              alternate,
              cdr(continuation)));
      }
      else
      {
        const auto consequent {
          compile(
            cadr(expression),
            syntactic_environment,
            frames,
            list(make<instruction>(mnemonic::JOIN)))
        };

        const auto alternate {
          cddr(expression)
            ? compile(
                caddr(expression),
                syntactic_environment,
                frames,
                list(
                  make<instruction>(mnemonic::JOIN)))
            : list(
                make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                make<instruction>(mnemonic::JOIN))
        };

        return
          compile(
            car(expression), // <test>
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::SELECT), consequent, alternate,
              continuation));
      }
    }

    /* ==== Lambda Expression =================================================
    *
    * <lambda expression> = (lambda <formals> <body>)
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(lambda)
    {
      debug(car(expression), console::faint, " ; is <formals>");

      if (in_a.program_declaration)
      {
        return
          cons(
            make<instruction>(mnemonic::LOAD_CLOSURE),
            body(
              cdr(expression),
              syntactic_environment,
              cons(
                car(expression), // <formals>
                frames),
              list(
                make<instruction>(mnemonic::RETURN)),
              as_program_declaration),
            continuation);
      }
      else
      {
        return
          cons(
            make<instruction>(mnemonic::LOAD_CLOSURE),
            body(
              cdr(expression),
              syntactic_environment,
              cons(
                car(expression), // <formals>
                frames),
              list(
                make<instruction>(mnemonic::RETURN))),
            continuation);
      }
    }

    /* ==== Call-With-Current-Continuation ====================================
    *
    * TODO documentation
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(call_cc)
    {
      debug(car(expression), console::faint, " ; is <procedure>");

      return
        cons(
          make<instruction>(mnemonic::LOAD_CONTINUATION),
          continuation,
          compile(
            car(expression),
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::CALL),
              continuation)));
    }

    /* ==== Fork ===============================================================
    *
    * TODO documentation
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(fork)
    {
      debug(car(expression), console::faint, " ; is <subprogram>");

      return
        cons(
          make<instruction>(mnemonic::FORK), cons(car(expression), frames),
          continuation);

      // return
      //   compile(
      //     car(expression),
      //     syntactic_environment,
      //     frames,
      //     cons(
      //       make<instruction>(mnemonic::FORK),
      //       continuation),
      //     as_program_declaration);
    }

    /* ==== Assignment ========================================================
    *
    * TODO documentation
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(assignment)
    {
      if (not expression)
      {
        throw syntax_error {"set!"};
      }
      else if (de_bruijn_index index {car(expression), frames}; index)
      {
        if (index.is_variadic())
        {
          debug(car(expression), console::faint, " ; is <variadic bound variable> references ", console::reset, index);

          return
            compile(
              cadr(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::STORE_VARIADIC), index,
                continuation));
        }
        else
        {
          debug(car(expression), console::faint, "; is a <bound variable> references ", console::reset, index);

          return
            compile(
              cadr(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::STORE_LOCAL), index,
                continuation));
        }
      }
      else
      {
        debug(car(expression), console::faint, "; is a <glocal variable>");

        return
          compile(
            cadr(expression),
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::STORE_GLOBAL), car(expression),
              continuation));
      }
    }

    /* ==== Explicit Variable Reference =======================================
    *
    * TODO DEPRECATED
    * TODO REMOVE AFTER IMPLEMENTED MODULE SYSTEM
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(reference)
    {
      if (not expression)
      {
        debug(car(expression), console::faint, " ; is <identifier> of itself");

        return unit;
      }
      else if (de_bruijn_index<
                 equivalence_comparator<2>
               > variable {
                 car(expression),
                 frames
               }; variable)
      {
        if (variable.is_variadic())
        {
          debug(car(expression), console::faint, " ; is <identifier> of local variadic ", console::reset, variable);

          return
            cons(
              make<instruction>(mnemonic::LOAD_VARIADIC), variable,
              continuation);
        }
        else
        {
          debug(car(expression), console::faint, " ; is <identifier> of local ", console::reset, variable);

          return
            cons(
              make<instruction>(mnemonic::LOAD_LOCAL), variable,
              continuation);
        }
      }
      else
      {
        debug(car(expression), console::faint, " ; is <identifier> of glocal variable");

        return
          cons(
            make<instruction>(mnemonic::LOAD_GLOBAL), car(expression),
            continuation);
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

