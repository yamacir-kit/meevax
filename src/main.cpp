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

#include <meevax/kernel/syntactic_continuation.hpp>

int main(const int argc, char const* const* const argv) try
{
  auto root = meevax::syntactic_continuation(meevax::layer<4>());

  root.configure(argc, argv);

  if (root.is_interactive_mode())
  {
    root.write_to(root.standard_interaction_port(), meevax::header(__func__), "You have control of root syntactic-continuation.\n");

    for (auto index = 0; root.ready(); ++index)
    {
      root.write_to(root.standard_interaction_port(), root.current_prompt());
      root.write_to(root.standard_interaction_port(), root.evaluate(root.read()), "\n");
    }

    root.write_to(root.standard_interaction_port(), meevax::header(__func__), "I have control of root syntactic-continuation.\n");
  }

  return meevax::underlying_cast(meevax::exit_status::success);
}

catch (meevax::exit_status const value)
{
  return meevax::underlying_cast(value);
}

/* ---- NOTE -------------------------------------------------------------------
 *
 *  Exceptions thrown by the run-time raise procedure reach here via the
 *  default-exception-handler (procedure '%throw'). Except procedure '%throw',
 *  any processing in namespace meevax::kernel must not throw meevax::object
 *  type object.
 *
 *  Perform I/O in the C++ way, as there may be a serious anomaly in system.
 *
 * -------------------------------------------------------------------------- */
catch (meevax::let const& error)
{
  std::cerr << meevax::header("exception-handler") << error << std::endl;
  std::cerr << meevax::header("exception-handler") << "Terminate the program without running any outstanding dynamic-wind after procedures." << std::endl;

  return underlying_cast(meevax::exit_status::failure);
}

/* ---- NOTE -------------------------------------------------------------------
 *
 *  Exceptions thrown from the Meevax system itself will eventually reach here.
 *
 * -------------------------------------------------------------------------- */
catch (meevax::error const& error)
{
  std::cerr << meevax::header("system-error") << error.what() << "." << std::endl;

  return underlying_cast(meevax::exit_status::failure);
}

catch (std::exception const& error)
{
  std::cerr << meevax::header("system-error") << error.what() << "." << std::endl;

  return underlying_cast(meevax::exit_status::failure);
}

catch (...)
{
  std::cerr << meevax::header("unknown-error") << "An unknown object was thrown that was neither a Meevax exception type nor a C++ standard exception type." << std::endl;

  return underlying_cast(meevax::exit_status::failure);
}

