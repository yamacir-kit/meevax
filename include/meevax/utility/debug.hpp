#ifndef INCLUDED_MEEVAX_UTILITY_DEBUG_HPP
#define INCLUDED_MEEVAX_UTILITY_DEBUG_HPP

#define LINE() std::cout << "; \x1b[36m" __FILE__ "\x1b[31m:\x1b[36m" << __LINE__ << "\x1b[0m" << std::endl

#define PRINT(...) std::cout << "; " #__VA_ARGS__ " = " << (__VA_ARGS__) << std::endl

#endif // INCLUDED_MEEVAX_UTILITY_DEBUG_HPP
