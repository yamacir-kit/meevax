/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
  export_spec::export_spec(object const& form)
    : form { form }
  {}

  auto export_spec::resolve(library & library) const -> object const&
  {
    auto identity = [&]()
    {
      assert(library.local().is<null>());

      if (form.is<pair>())
      {
        assert(form[0].is<symbol>());
        assert(form[0].as<symbol>() == "rename");
        assert(form[1].is_also<identifier>());
        assert(form[2].is_also<identifier>());
        return make<absolute>(form[2], library.identify(form[1], unit));
      }
      else
      {
        assert(form.is_also<identifier>());
        return library.identify(form, unit);
      }
    };

    return library.subset = cons(identity(), library.subset);
  }
} // namespace kernel
} // namespace meevax


