#ifndef INCLUDED_MEEVAX_SYSTEM_FILE_HPP
#define INCLUDED_MEEVAX_SYSTEM_FILE_HPP

#include <fstream>
#include <iostream>
#include <string>
#include <utility>

// TODO ファイルパスを std::string からランタイムストリングかパス型へ変更

namespace meevax::system
{
  struct file
    : public std::fstream
  {
    const std::string path;

    file(const std::string& path)
      : std::fstream {path}
      , path {path}
    {}

    template <typename... Ts>
    explicit constexpr file(Ts&&... xs)
      : std::fstream {std::forward<Ts>(xs)...}
    {}
  };

  auto operator<<(std::ostream& os, const file& file)
    -> decltype(os)
  {
    return os << "\x1b[36m#(file \"" << file.path << "\")\x1b[0m";
  }

  struct input_file
    : public std::ifstream
  {
    const std::string path;

    input_file(const std::string& path)
      : std::ifstream {path}
      , path {path}
    {}

    template <typename... Ts>
    explicit constexpr input_file(Ts&&... xs)
      : std::ifstream {std::forward<Ts>(xs)...}
    {}
  };

  auto operator<<(std::ostream& os, const input_file& file)
    -> decltype(os)
  {
    return os << "\x1b[36m#(input-file \"" << file.path << "\")\x1b[0m";
  }

  struct output_file
    : public std::ofstream
  {
    const std::string path;

    output_file(const std::string& path)
      : std::ofstream {path}
      , path {path}
    {}

    template <typename... Ts>
    explicit constexpr output_file(Ts&&... xs)
      : std::ofstream {std::forward<Ts>(xs)...}
    {}
  };

  auto operator<<(std::ostream& os, const output_file& file)
    -> decltype(os)
  {
    return os << "\x1b[36m#(output-file \"" << file.path << "\")\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_FILE_HPP

