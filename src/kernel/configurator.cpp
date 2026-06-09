/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <regex>

namespace meevax::inline kernel
{
  struct option
  {
    char c;

    std::string s;

    std::string operand;

    std::string description;

    std::regex const pattern;

    std::function<auto (std::function<auto () -> object> const&) -> void> evaluate;

    explicit option(char c, std::string const& s, std::string const& operand, std::string const& description, auto&& f)
      : c           { c }
      , s           { s }
      , operand     { operand }
      , description { description }
      , pattern     { not c ? s : std::string("(") + c + '|' + s + ')' }
      , evaluate    { std::forward<decltype(f)>(f) }
    {
      assert(not s.empty());
    }
  };

  auto format(std::string const& text, std::string indent = std::string(10, ' '), std::size_t column_max = 80)
  {
    auto input = std::istringstream(text);

    auto output = std::ostringstream(indent, std::ios_base::ate);

    auto column = indent.size();

    for (auto word = std::string(); input >> word; )
    {
      if (indent.size() < column)
      {
        if (column_max < column + 1 + word.size())
        {
          output << '\n' << indent << word;
          column = indent.size() + word.size();
        }
        else
        {
          output << ' ' << word;
          column += 1 + word.size();
        }
      }
      else
      {
        output << word;
        column += word.size();
      }
    }

    return output.str();
  }

  auto options() -> std::vector<option> const&
  {
    auto const static options = std::vector<option>
    {
      option('A', "append-library-directory", "<directory>",
             "Append <directory> to the list of directories that are searched in order to locate imported libraries. [SRFI 138]",
      [](auto read)
      {
        configurator::directories().emplace_back(std::filesystem::weakly_canonical(lexical_cast(read())));
      }),

      option('D', "add-feature-identifier", "<name>",
             "Add <name> to the list of feature identifiers. [SRFI 138]",
      [](auto read)
      {
        features() = cons(read(), features());
      }),

      option('I', "prepend-library-directory", "<directory>",
             "Prepend <directory> to the list of directories that are searched in order to locate imported libraries. [SRFI 138]",
      [](auto read)
      {
        configurator::directories().emplace_front(std::filesystem::weakly_canonical(lexical_cast(read())));
      }),

      option('\0', "color", "<boolean>",
             "Colorize external representations. "
             "By default, colorization is enabled automatically for terminal output. "
             "#t always enables colorization; #f always disables it.",
      [](auto read)
      {
        if (let const x = read(); x.is<bool>())
        {
          configurator::color() = x;
        }
        else
        {
          throw error(make<string>("invalid argument for option '--color'"), x);
        }
      }),

      option('e', "evaluate", "<datum>",
             "Read and evaluate <datum> in the interaction environment.",
      [](auto read)
      {
        interaction_environment().as<environment>().evaluate(read());
      }),

      option('h', "help", "",
             "Display this help message and exit.",
      [](auto)
      {
        std::cout << bold("Usage:") << "\n"
                  << "  meevax [" << underline("options") << "...] [" << underline("files") << "...]\n"
                  << "\n"
                  << bold("Options:") << "\n";

        for (auto const& option : meevax::options())
        {
          if (option.c)
          {
            std::cout << "  " << bold('-', option.c) << ", " << bold("--", option.s);
          }
          else
          {
            std::cout << "      " << bold("--", option.s);
          }

          if (not option.operand.empty())
          {
            std::cout << ' ' << underline(option.operand);
          }

          std::cout << '\n' << format(option.description) << "\n\n";
        }

        std::cout << bold("Examples:") << "\n"
                     "  " << bold("Normalize the first datum from a file:") << "\n"
                     "\n"
                     "    $ meevax --color=#f -w \"$(cat ./path/to/file.ss)\"\n"
                     "\n"
                     "  " << bold("Write the feature identifiers:") << "\n"
                     "\n"
                     "    $ meevax -e '(import (scheme base) (scheme write))' \\\n"
                     "             -e '(write (features))'\n"
                     ;

        std::cout << std::flush;
      }),

      option('i', "interactive", "",
             "Enter the REPL session after evaluating any <file>s given as command-line arguments.",
      [](auto)
      {
        configurator::interactive() = true;
      }),

      option('l', "load", "<file>",
             "Load <file> into the interaction environment.",
      [](auto read)
      {
        interaction_environment().as<environment>().load(lexical_cast(read()));
      }),

      option('\0', "library-directories", "",
             "Display the list of directories that are searched in order to locate imported libraries.",
      [](auto)
      {
        for (auto const& directory : configurator::directories())
        {
          std::cout << directory.native() << std::endl;
        }
      }),

      option('v', "version", "",
             "Display version and exit.",
      [](auto)
      {
        std::cout << version() << std::endl;
      }),

      option('w', "write", "<datum>",
             "Read <datum> and write it to standard output.",
      [](auto read)
      {
        std::cout << read() << std::endl;
      }),
    };

    return options;
  }

  auto configurator::color() -> object &
  {
    let static color = unspecified;
    return color;
  }

  auto configurator::command_line() -> std::vector<std::string> &
  {
    auto static command_line = std::vector<std::string>();
    return command_line;
  }

  auto configurator::configure(int const argc, char const* const* const argv) -> void
  {
    for (auto i = 0; i < argc; ++i)
    {
      command_line().emplace_back(argv[i]);
    }

    return configure(command_line());
  }

  auto configurator::configure(std::vector<std::string> const& args) -> void
  {
    auto const static pattern = std::regex(R"(--(\w[-\w]+)(=(.*))?|-([\w]+))");

    auto evaluator = [&](auto&& name) -> decltype(auto)
    {
      if (auto iter = std::find_if(options().begin(), options().end(), [&](auto&& option)
                      {
                        return std::regex_match(name, option.pattern);
                      });
          iter != options().end())
      {
        return iter->evaluate;
      }
      else
      {
        throw error(make<string>("unknown option"), make<symbol>(name));
      }
    };

    for (auto iter = std::next(args.begin()); iter != args.end(); ++iter)
    {
      static std::regex const pattern { R"(--(\w[-\w]+)(?:=(.*))?|-([\w]+))" };

      if (std::smatch result; std::regex_match(*iter, result, pattern))
      {
        auto read = [&]()
        {
          if (std::next(iter) != args.end())
          {
            return input_string_port(*++iter).read();
          }
          else
          {
            throw error(make<string>("an argument required but not specified"),
                        make<symbol>(*iter));
          }
        };

        if (result.length(3))
        {
          for (auto str3 = result.str(3); not str3.empty(); str3.erase(0, 1))
          {
            std::invoke(evaluator(str3.substr(0, 1)), read);
          }
        }
        else if (result.length(2))
        {
          std::invoke(evaluator(result.str(1)), [result]()
          {
            return input_string_port(result.str(2)).read();
          });
        }
        else if (result.length(1))
        {
          std::invoke(evaluator(result.str(1)), read);
        }
      }
      else
      {
        environment().load(*iter);
      }
    }
  }

  auto configurator::directories() -> std::list<std::filesystem::path> &
  {
    auto static directories = std::list<std::filesystem::path> { user_library_directory(), system_library_directory() };
    return directories;
  }

  auto configurator::interactive() -> bool &
  {
    auto static interactive = false;
    return interactive;
  }
} // namespace meevax::kernel
