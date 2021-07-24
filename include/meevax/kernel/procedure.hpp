#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <memory>
#include <utility>

#if __unix__
#include <dlfcn.h> // dlopen, dlclose, dlerror
#else
#error
#endif

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/linker.hpp> // TODO REMOVE
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  #if __has_cpp_attribute(maybe_unused)
  #define PROCEDURE(...) meevax::let const __VA_ARGS__([[maybe_unused]] meevax::let const& xs)
  #else
  #define PROCEDURE(...) meevax::let const __VA_ARGS__(                 meevax::let const& xs)
  #endif

  static auto close = [](pointer<void> const handle)
  {
    if (handle and ::dlclose(handle))
    {
      std::cerr << dlerror() << std::endl;
    }
  };

  using library_handle = std::unique_ptr<void, decltype(close)>;

  auto from(std::string const& library_name) -> library_handle const& // NOTE: library_name = "lib*.so"
  {
    static std::unordered_map<std::string, library_handle> dynamic_libraries {};

    dlerror(); // clear

    try
    {
      return dynamic_libraries.at(library_name);
    }
    catch (std::out_of_range const&)
    {
      if (auto handle = library_handle(dlopen(library_name.c_str(), RTLD_LAZY | RTLD_GLOBAL), close); handle)
      {
        dynamic_libraries.emplace(library_name, std::move(handle));

        return from(library_name);
      }
      else
      {
        throw file_error(make<string>(dlerror()), unit);
      }
    }
  }

  struct procedure : public std::function<PROCEDURE()>
  {
    using signature = PROCEDURE((*));

    std::string const name;

    explicit procedure(std::string const& name, std::function<PROCEDURE()> const& function)
      : std::function<PROCEDURE()> { function  }
      , name { name }
    {}

    explicit procedure(std::string const& function_name, library_handle const& handle)
      : std::function<PROCEDURE()> { link_as<signature>(function_name, handle) }
      , name { function_name }
    {}

    virtual ~procedure() = default;

    template <typename T, REQUIRES(std::is_pointer<T>)>
    static auto link_as(std::string const& symbol_name, library_handle const& handle) -> T
    {
      if (pointer<void> const address = dlsym(handle.get(), symbol_name.c_str()); address)
      {
        return reinterpret_cast<T>(address);
      }
      else
      {
        throw file_error(make<string>(dlerror()), unit);
      }
    }
  };

  auto operator <<(output_port & port, procedure const& datum) -> output_port &;

  template <typename T>
  struct is
  {
    let const& operator ()(let const& xs) const
    {
      auto is_T = [](let const& x)
      {
        return x.is<T>();
      };

      return std::all_of(std::begin(xs), std::end(xs), is_T) ? t : f;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
