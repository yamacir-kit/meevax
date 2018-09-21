#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <iostream>
#include <memory>
#include <string>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/alias.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/utility/binder.hpp>

namespace meevax::lisp
{
  class cell
  {
    const std::shared_ptr<cell> car_, cdr_;

  public:
    explicit cell() noexcept = default;

    explicit cell(const std::shared_ptr<cell>& car)
      : car_ {car}
    {}

    explicit cell(const std::shared_ptr<cell>& car,
                  const std::shared_ptr<cell>& cdr)
      : car_ {car},
        cdr_ {cdr}
    {}

  public:
    template <typename T, typename... Ts>
    static auto make_as(Ts&&... xs)
      -> const std::shared_ptr<cell>
    {
      return std::make_shared<
               meevax::utility::binder<T, cell>
             >(std::forward<Ts>(xs)...);
    }

    template <typename T>
    auto as() const noexcept try
    {
      return dynamic_cast<const T&>(*this);
    }
    catch (const std::bad_cast& error)
    {
      std::cerr << error("dynamic dispatch failed for \"" << type().name() << "\"") << std::endl;
      std::exit(boost::exit_exception_failure);
    }

    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(cell);
    }

  public:
    static inline const auto nil {make_as<symbol>("nil")};

    friend bool atom(const std::shared_ptr<cell>& e)
    {
      static const std::unordered_map<std::type_index, bool> dispatch
      {
        {typeid(symbol), true}
      };

      return !e->cdr_ && dispatch.at(e->type());
    }

    friend auto car(const std::shared_ptr<cell>& e) noexcept
      -> decltype(auto)
    {
      return (e && e->car_) ? e->car_ : nil;
    }

    friend auto cdr(const std::shared_ptr<cell>& e) noexcept
      -> decltype(auto)
    {
      return (e && e->cdr_) ? e->cdr_ : nil;
    }

  public:
    friend auto operator<<(std::ostream& os, const std::shared_ptr<cell>& e)
      -> decltype(os)
    {
      if (e->type() == typeid(cell))
      {
        return os << "(" << e->car_ << " . " << e->cdr_ << ")";
      }
      else if (e->type() == typeid(symbol))
      {
        return os << e->as<symbol>();
      }
      else
      {
        throw std::runtime_error {std::to_string(__LINE__)};
      }
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

