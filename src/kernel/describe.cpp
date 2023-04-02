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

#include <meevax/kernel/describe.hpp>
#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
  auto describe(object const& target_variable_name, object const& target_library_name) -> void
  {
    if (target_variable_name.is<symbol>())
    {
      std::map<std::string, object> search_result {};

      for (auto&& [library_name, library] : libraries)
      {
        if (target_library_name.is<null>() or lexical_cast<std::string>(target_library_name) == library_name)
        {
          for (let const& identity : library.resolve())
          {
            if (identity.as<absolute>().symbol() == target_variable_name)
            {
              search_result.emplace(library_name, identity.as<absolute>().load());
            }
          }
        }
      }

      for (auto&& [library_name, object] : search_result)
      {
        std::cout << object << " from library " << library_name << std::endl;
      }
    }
    else if (target_variable_name.is<string>())
    {
      std::map<std::string, std::vector<object>> search_result {};

      auto const pattern = std::regex(static_cast<std::string>(target_variable_name.as<string>()));

      for (auto&& [library_name, library] : libraries)
      {
        if (target_library_name.is<null>() or lexical_cast<std::string>(target_library_name) == library_name)
        {
          for (let const& identity : library.resolve())
          {
            if (std::regex_match(identity.as<absolute>().symbol().as<symbol>().std_string, pattern))
            {
              search_result[library_name].push_back(identity);
            }
          }
        }

        if (search_result.find(library_name) != std::end(search_result))
        {
          std::sort(std::begin(search_result[library_name]), std::end(search_result[library_name]), [](let const& a, let const& b)
          {
            return a.as<absolute>().symbol().as<symbol>() < b.as<absolute>().symbol().as<symbol>();
          });
        }
      }

      for (auto&& [library_name, identities] : search_result)
      {
        std::cout << "from library " << library_name << std::endl;

        for (auto&& identity : identities)
        {
          std::cout << "  " << identity.as<absolute>().symbol() << std::endl;
        }

        std::cout << std::endl;
      }
    }
  }
} // namespace kernel
} // namespace meevax
