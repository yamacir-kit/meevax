/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#include <meevax/kernel/mnemonic.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & os, mnemonic const& datum) -> std::ostream &
  {
    switch (datum)
    {
      case mnemonic::call:              return os << "call";
      case mnemonic::cons:              return os << "cons";
      case mnemonic::define:            return os << "define";
      case mnemonic::define_syntax:     return os << "define-syntax";
      case mnemonic::drop:              return os << "drop";
      case mnemonic::dummy:             return os << "dummy";
      case mnemonic::join:              return os << "join";
      case mnemonic::let_syntax:        return os << "let-syntax";
      case mnemonic::letrec:            return os << "letrec";
      case mnemonic::letrec_syntax:     return os << "letrec-syntax";
      case mnemonic::load_absolute:     return os << "load-absolute";
      case mnemonic::load_auxiliary:    return os << "load-auxiliary";
      case mnemonic::load_closure:      return os << "load-closure";
      case mnemonic::load_constant:     return os << "load-constant";
      case mnemonic::load_continuation: return os << "load-continuation";
      case mnemonic::load_relative:     return os << "load-relative";
      case mnemonic::load_variadic:     return os << "load-variadic";
      case mnemonic::return_:           return os << "return";
      case mnemonic::select:            return os << "select";
      case mnemonic::stop:              return os << "stop";
      case mnemonic::store_absolute:    return os << "store-absolute";
      case mnemonic::store_auxiliary:   return os << "store-auxiliary";
      case mnemonic::store_relative:    return os << "store-relative";
      case mnemonic::store_variadic:    return os << "store-variadic";
      case mnemonic::tail_call:         return os << "tail-call";
      case mnemonic::tail_select:       return os << "tail-select";

      default:
        throw std::logic_error(__func__);
    }
  }
} // namespace kernel
} // namespace meevax
