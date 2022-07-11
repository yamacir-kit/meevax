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

#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  struct vector
  {
    std::vector<value_type> data;

    explicit vector() = default;

    explicit vector(meevax::string const&);

    /*
       (vector obj ...)                                               procedure

       Returns a newly allocated vector whose elements contain the given
       arguments. It is analogous to list.
    */
    explicit vector(const_reference);

    /*
       (make-vector k)                                                procedure
       (make-vector k fill)                                           procedure

       Returns a newly allocated vector of k elements. If a second argument is
       given, then each element is initialized to fill. Otherwise the initial
       contents of each element is unspecified.
    */
    explicit vector(const_reference, const_reference);

    /*
       (vector-append vector ...)                                     procedure

       Returns a newly allocated vector whose elements are the concatenation of
       the elements of the given vectors.
    */
    auto append(const_reference) -> void;

    /*
       (vector-copy vector)                                           procedure
       (vector-copy vector start)                                     procedure
       (vector-copy vector start end)                                 procedure

       Returns a newly allocated copy of the elements of the given vector
       between start and end. The elements of the new vector are the same (in
       the sense of eqv?) as the elements of the old.
    */
    auto copy(const_reference, const_reference) const -> value_type;

    /*
       (vector-copy! to at from)                                      procedure
       (vector-copy! to at from start)                                procedure
       (vector-copy! to at from start end)                            procedure

       It is an error if at is less than zero or greater than the length of to.
       It is also an error if (- (vector-length to) at) is less than
       (- end start).

       Copies the elements of vector from between start and end to vector to,
       starting at at. The order in which elements are copied is unspecified,
       except that if the source and destination overlap, copying takes place
       as if the source is first copied into a temporary vector and then into
       the destination. This can be achieved without allocating storage by
       making sure to copy in the correct direction in such circumstances.
    */
    auto copy(const_reference, const_reference, const_reference, const_reference) -> void;

    /*
       (vector-fill! vector fill)                                     procedure
       (vector-fill! vector fill start)                               procedure
       (vector-fill! vector fill start end)                           procedure

       The vector-fill! procedure stores fill in the elements of vector between
       start and end.
    */
    auto fill(const_reference, const_reference, const_reference) -> void;

    /*
       (vector-length vector)                                         procedure

       Returns the number of elements in vector as an exact integer.
    */
    auto length() const -> value_type;

    /*
       (vector->list vector)                                          procedure
       (vector->list vector start)                                    procedure
       (vector->list vector start end)                                procedure
       (list->vector list)                                            procedure

       The vector->list procedure returns a newly allocated list of the objects
       contained in the elements of vector between start and end. The
       list->vector procedure returns a newly created vector initialized to the
       elements of the list list.

       In both procedures, order is preserved.
    */
    auto list(const_reference, const_reference) const -> value_type;

    /*
       (vector-ref vector k)                                          procedure

       It is an error if k is not a valid index of vector. The vector-ref
       procedure returns the contents of element k of vector.
    */
    auto ref(const_reference) const -> const_reference;

    /*
       (vector-set! vector k obj)                                     procedure

       It is an error if k is not a valid index of vector. The vector-set!
       procedure stores obj in element k of vector.
    */
    auto set(const_reference, const_reference) -> const_reference;

    /*
       (vector->string vector)                                        procedure
       (vector->string vector start)                                  procedure
       (vector->string vector start end)                              procedure

       (string->vector string)                                        procedure
       (string->vector string start)                                  procedure
       (string->vector string start end)                              procedure

       It is an error if any element of vector between start and end is not a
       character.

       The vector->string procedure returns a newly allocated string of the
       objects contained in the elements of vector between start and end. The
       string->vector procedure returns a newly created vector initialized to
       the elements of the string string between start and end.

       In both procedures, order is preserved.
    */
    auto string(const_reference, const_reference) const -> value_type;
  };

  auto operator ==(vector const&, vector const&) -> bool;

  auto operator <<(std::ostream &, vector const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
