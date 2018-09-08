#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <iostream>
#include <memory>
#include <string>
#include <typeinfo>
#include <utility>

#include <meevax/utility/type_erasure.hpp>

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
      return std::make_shared<cell>(
               std::make_shared<
                 meevax::utility::binder<T, cell>
               >(std::forward<Ts>(xs)...)
             );
    }

    template <typename T>
    auto as() const noexcept
      -> const T&
    {
      static const T dummy {"dummy"};
      return dynamic_cast<const T*>(this) ? dynamic_cast<const T&>(*this) : dummy;
    }

    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(cell);
    }

  public:
    static inline const auto nil {make_as<std::string>("nil")};

    friend bool atom(const std::shared_ptr<cell>& e) noexcept
    {
      return e && !e->cdr_;
    }

    friend auto car(const std::shared_ptr<cell>& e) noexcept
      -> decltype(auto)
    {
      return e && e->car_ ? e->car_ : nil;
    }

    friend auto cdr(const std::shared_ptr<cell>& e) noexcept
      -> decltype(auto)
    {
      return e && e->cdr_ ? e->cdr_ : nil;
    }

  public:
    friend auto operator<<(std::ostream& os, const std::shared_ptr<cell>& e)
      -> decltype(os)
    {
      if (!e->cdr_)
      {
        return os << e->car_->as<std::string>();
      }
      else
      {
        return os << "(" << e->car_ << " . " << e->cdr_ << ")";
      }
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

