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

#ifndef INCLUDED_MEEVAX_UTILITY_DEBUG_HPP
#define INCLUDED_MEEVAX_UTILITY_DEBUG_HPP

#include <iomanip>
#include <iostream>

#define LINE() \
  std::cerr << "; \x1b[33m" __FILE__ "\x1b[31m:\x1b[36m" << __LINE__ << "\x1b[0m" << std::endl

#define PRINT(...) \
  std::cerr << "; " #__VA_ARGS__ " = " << std::boolalpha << (__VA_ARGS__) << std::endl

#endif // INCLUDED_MEEVAX_UTILITY_DEBUG_HPP
