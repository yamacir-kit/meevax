/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_KERNEL_ERROR_HPP
#define INCLUDED_MEEVAX_KERNEL_ERROR_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/type_traits/underlying_cast.hpp>

#include <stdexcept>

/* ---- Error ------------------------------------------------------------------
 *
 * - error
 *    |-- file-error
 *    |-- read_error
 *    |    `-- tagged_read_error
 *    `-- syntax_error
 *
 * -------------------------------------------------------------------------- */

namespace meevax
{
inline namespace kernel
{
  enum class exit_status : int
  {
    success = boost::exit_success,
    failure = boost::exit_failure,
  };

  struct error
    : public virtual pair
  {
    using pair::pair;

    virtual auto what() const -> std::string
    {
      std::stringstream ss {};

      ss << "error: ";

      car(*this).as<const string>().write_string(ss);

      if (cdr(*this))
      {
        ss << ": " << cdr(*this);
      }

      return ss.str();
    }

    virtual void raise() const
    {
      throw *this;
    }
  };

  auto operator <<(output_port & port, error const& datum) -> output_port &;

  #define DEFINE_ERROR(TYPENAME)                                               \
  struct TYPENAME ## _error : public error                                     \
  {                                                                            \
    using error::error;                                                        \
                                                                               \
    ~TYPENAME ## _error() override = default;                                  \
  }

  DEFINE_ERROR(file);
  DEFINE_ERROR(read);
  DEFINE_ERROR(syntax);

  template <typename... Ts>
  struct tagged_read_error : public read_error
  {
    using read_error::read_error;

    ~tagged_read_error() override = default;
  };

  template <typename... Ts>
  struct tagged_syntax_error : public syntax_error
  {
    using syntax_error::syntax_error;

    ~tagged_syntax_error() override = default;
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ERROR_HPP
