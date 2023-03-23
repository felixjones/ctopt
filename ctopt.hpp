/**
 * Crappy C++20 options parser
 *
 * MIT License
 *
 * Copyright (c) 2022-2023 Felix Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Example:
 *
 * ```C++
 * #include "ctopt.hpp"
 *
 * int main(int argc, char* argv[]) {
 *     using namespace ctopt;
 *
 *     // Define options
 *     static constexpr auto get_opts = make_options(
 *         option('v', "verbose")
 *     );
 *
 *     // Parse program arguments
 *     const auto args = get_opts(argc, argv);
 *     if (!args) {
 *         const auto error_text = args.error_str();
 *         const auto help_text = get_opts.help_str();
 *         // ...
 *     }
 *
 *     const auto verbose = args.get<bool>("verbose");
 *     if (verbose) {
 *         // Print verbose output
 *         // ...
 *     }
 *
 *     // ...
 * }
 * ```
 */

#ifndef CTOPT_HPP
#define CTOPT_HPP

#include <algorithm>
#include <array>
#include <cctype>
#include <cstddef>
#include <charconv>
#include <concepts>
#include <iterator>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace ctopt {

    namespace detail {

        template <typename T>
        concept String = std::constructible_from<std::string_view, T>;

        template <typename T>
        concept Array = std::same_as<std::array<typename T::value_type, sizeof(T) / sizeof(typename T::value_type)>, T>;

        template <Array T>
        inline constexpr auto array_size = sizeof(T) / sizeof(typename T::value_type);

        template <typename T>
        concept Optional = std::same_as<std::optional<typename T::value_type>, T>;

        template <typename T>
        concept Pair = std::same_as<std::pair<typename T::first_type, typename T::second_type>, T>;

        template <typename T>
        struct is_tuple : std::false_type {};

        template <typename... T>
        struct is_tuple<std::tuple<T...>> : std::true_type {};

        template <typename T>
        concept Tuple = is_tuple<T>::value;

        template <typename T>
        concept Numeric = std::is_arithmetic_v<T> && !std::is_same_v<T, bool> && !std::is_same_v<T, char>;

        template <typename T>
        concept Bool = std::same_as<T, bool>;

        template <typename T>
        concept StandardContainer = requires(T a, const T b) {
            requires std::regular<T>;
            requires std::swappable<T>;
            requires std::destructible<typename T::value_type>;
            requires std::forward_iterator<typename T::iterator>;
            requires std::forward_iterator<typename T::const_iterator>;
            requires std::signed_integral<typename T::difference_type>;
            requires std::same_as<typename T::difference_type, typename std::iterator_traits<typename T::iterator>::difference_type>;
            requires std::same_as<typename T::difference_type, typename std::iterator_traits<typename T::const_iterator>::difference_type>;
            { a.begin() } -> std::same_as<typename T::iterator>;
            { a.end() } -> std::same_as<typename T::iterator>;
            { a.cbegin() } -> std::same_as<typename T::const_iterator>;
            { a.cend() } -> std::same_as<typename T::const_iterator>;
            { a.size() } -> std::same_as<typename T::size_type>;
            { a.max_size() } -> std::same_as<typename T::size_type>;
            { a.empty() } -> std::same_as<bool>;
            { b.begin() } -> std::same_as<typename T::const_iterator>;
            { b.end() } -> std::same_as<typename T::const_iterator>;
        } && !Array<T>;

        template <typename T>
        concept ReservableContainer = requires(T a) {
            { a.reserve(typename T::size_type{}) } -> std::same_as<void>;
            { std::back_inserter(a) } -> std::same_as<std::back_insert_iterator<T>>;
        };

        template <typename T>
        concept ShrinkableContainer = requires(T a) {
            { a.shrink_to_fit() } -> std::same_as<void>;
        };

        template <typename T>
        concept RangeConstructable = requires(std::vector<typename T::value_type> v) {
            { T{std::cbegin(v), std::cend(v)} } -> std::same_as<T>;
        };

        template <typename T>
        concept Containable = std::same_as<const char*, T>
                              || std::same_as<const std::string*, T>
                              || RangeConstructable<T>
                              || Array<T>
                              || Optional<T>
                              || Pair<T>
                              || Tuple<T>;

        template <typename T>
        concept NotContainable = !Containable<T>;

        template <std::size_t I, typename T>
        struct containable_element;

        template <std::size_t I, Tuple T>
        struct containable_element<I, T> : std::tuple_element<I, T> {};

        template <std::size_t I, Pair T>
        struct containable_element<I, T> : std::conditional<I == 0, typename T::first_type,
                std::conditional_t<I == 1, typename T::second_type, std::false_type>> {};

        template <std::size_t I, Optional T>
        struct containable_element<I, T> : std::conditional<I == 0, typename T::value_type, std::false_type> {};

        template <std::size_t I, Array T>
        struct containable_element<I, T> : std::conditional<(I < array_size<T>), typename T::value_type, std::false_type> {};

        template <std::size_t I, std::same_as<const std::string*> T>
        struct containable_element<I, T> : std::conditional<I == 0, const std::string, std::false_type> {};

        template <std::size_t I, std::same_as<const char*> T>
        struct containable_element<I, T> : std::conditional<I == 0, const char, std::false_type> {};

        template <std::size_t I, StandardContainer T>
        struct containable_element<I, T> {
            using type = typename T::value_type;
        };

        template <std::size_t I, typename T>
        using containable_element_t = typename containable_element<I, T>::type;

        template <typename T>
        struct containable_size;

        template <Tuple T>
        struct containable_size<T> : std::tuple_size<T> {};

        template <Pair T>
        struct containable_size<T> : std::integral_constant<std::size_t, 2> {};

        template <Optional T>
        struct containable_size<T> : std::integral_constant<std::size_t, 1> {};

        template <Array T>
        struct containable_size<T> : std::integral_constant<typename T::size_type, array_size<T>> {};

        template <std::same_as<const std::string*> T>
        struct containable_size<T> : std::integral_constant<std::size_t, 1> {};

        template <std::same_as<const char*> T>
        struct containable_size<T> : std::integral_constant<std::size_t, 1> {};

        template <typename T>
        inline constexpr auto containable_size_v = containable_size<T>::value;

    } // detail

    namespace {

        /// Parse string to a boolean value
        /// \tparam T Bool type
        /// \param sv View into string
        /// \return Boolean representation of string
        template <detail::Bool T>
        auto from_string(std::string_view sv) noexcept -> T {
            if (sv.empty()) {
                return false;
            }

            std::string lower{};
            lower.reserve(sv.size());
            std::transform(sv.cbegin(), sv.cend(), std::back_inserter(lower), [](char c){
                return std::tolower(c);
            });

            if (lower == "true" || lower == "on" || lower == "yes" || lower == "y") {
                return true;
            }

            int value{};
            auto [ptr, ec] { std::from_chars(&sv.front(), &sv.back(), value) };
            if (ptr == &sv.back()) {
                return bool(value);
            }

            return false;
        }

        /// Parse string to a numeric value
        /// \tparam T Numeric type
        /// \param sv View into string
        /// \return Numeric representation of string
        template <detail::Numeric T>
        auto from_string(std::string_view sv) noexcept -> T {
            if (sv.empty()) {
                return {};
            }

            T value{};
            auto [ptr, ec] { std::from_chars(std::to_address(sv.cbegin()), std::to_address(sv.cend()), value) };
            if (ptr == std::to_address(sv.cend())) {
                return value;
            }

            return {};
        }

        /// Parse string to a character value
        /// \tparam T Numeric type
        /// \param sv View into string
        /// \return First character of string
        template <std::same_as<char> T>
        auto from_string(std::string_view sv) noexcept -> T {
            if (sv.empty()) {
                return 0;
            }

            return sv.front();
        }

        /// Construct view into string
        /// \tparam T std::string_view
        /// \param s String to view into
        /// \return View into string
        template <std::same_as<std::string_view> T>
        auto from_string(const std::same_as<std::string> auto& s) noexcept -> T {
            return std::string_view{s};
        }

        /// Get pointer to string
        /// \tparam T const std::string*
        /// \param s String to return pointer of
        /// \return Pointer to string
        template <std::same_as<const std::string*> T>
        auto from_string(const std::same_as<std::string> auto& s) noexcept -> T {
            return &s;
        }

        /// Get C-string pointer
        /// \tparam T const char*
        /// \param s String to return C-string data of
        /// \return Pointer to C-string
        template <std::same_as<const char*> T>
        auto from_string(const std::string& s) noexcept -> T {
            return s.c_str();
        }
    }

    namespace detail {

        template <typename T>
        auto forward_arg(auto& rhs) noexcept {
            if constexpr (String<T>) {
                return rhs;
            } else {
                return from_string<T>(rhs);
            }
        }

        template <std::size_t I = 0>
        void tuple_insert(auto& tuple, auto it, auto end) noexcept {
            using tuple_type = std::remove_cvref_t<decltype(tuple)>;
            using element_type = containable_element_t<I, tuple_type>;

            if (it == end) {
                return;
            }

            std::get<I>(tuple) = forward_arg<element_type>(*it++);

            if constexpr (I + 1 < containable_size_v<tuple_type>) {
                tuple_insert<I + 1>(tuple, it, end);
            }
        }

        template <Tuple T>
        auto from_args(const std::vector<std::string>& v) noexcept -> T {
            T t{};
            tuple_insert(t, v.cbegin(), v.cend());
            return t;
        }

        template <Pair T>
        auto from_args(const std::vector<std::string>& v) noexcept -> T {
            using first_type = typename T::first_type;
            using second_type = typename T::second_type;

            if (v.size() >= 2) {
                return std::make_pair(
                        forward_arg<first_type>(v.front()),
                        forward_arg<second_type>(v.at(1))
                );
            }
            if (v.size() == 1) {
                return std::make_pair(
                        forward_arg<first_type>(v.front()),
                        second_type{}
                );
            }
            return {};
        }

        template <Optional T>
        auto from_args(const std::vector<std::string>& v) noexcept -> T {
            if (v.empty()) {
                return std::nullopt;
            }

            return forward_arg<typename T::value_type>(v.front());
        }

        template <Array T>
        auto from_args(const std::vector<std::string>& v) noexcept -> T {
            T t{};
            if constexpr (std::same_as<std::string, typename T::value_type>) {
                std::copy_n(v.cbegin(), containable_size_v<T>, t.begin());
            } else {
                std::generate_n(t.begin(), containable_size_v<T>, [it = v.cbegin()]() mutable {
                    return from_string<typename T::value_type>(*it++);
                });
            }
            return t;
        }

        template <std::same_as<const std::string*> T>
        auto from_args(const std::vector<std::string>& v) noexcept -> T {
            if (v.empty()) {
                return nullptr;
            }

            return v.data();
        }

        template <std::same_as<const char*> T>
        auto from_args(const std::vector<std::string>& v) noexcept -> T {
            if (v.empty()) {
                return nullptr;
            }

            return v.front().c_str();
        }

        template <StandardContainer T>
        auto from_args(const std::vector<std::string>& v) noexcept -> T {
            if constexpr (std::same_as<std::vector<std::string>, T>) {
                return v;
            } else if constexpr (std::same_as<std::string, typename T::value_type> && RangeConstructable<T>) {
                return T{v.cbegin(), v.cend()};
            } else if constexpr (std::same_as<char, typename T::value_type>) {
                T a{};
                auto inserter = std::back_inserter(a);
                for (const auto& s : v) {
                    std::copy(s.cbegin(), s.cend(), inserter);
                }
                if constexpr (ShrinkableContainer<T>) {
                    a.shrink_to_fit();
                }
                return a;
            } else {
                T a{};
                if constexpr (ReservableContainer<T>) {
                    a.reserve(v.size());
                }
                std::transform(v.cbegin(), v.cend(), std::back_inserter(a), [](const auto& str) {
                    return from_string<typename T::value_type>(str);
                });
                return a;
            }
        }

        template <NotContainable T>
        auto from_args(const std::vector<std::string>& v) noexcept -> T {
            return from_string<T>(v.front());
        }

        struct name_pair {
            auto operator<=>(const name_pair& rhs) const = default;

            [[nodiscard]]
            auto printable() const noexcept -> std::string {
                if (!long_name.empty()) {
                    return "--" + std::string{long_name};
                }
                return "-" + std::string{1, short_name};
            }

            const char short_name;
            const std::string_view long_name;
        };

        template <bool NoError, std::size_t N>
        struct parse_context;

    } // namespace detail

    template <bool NoError, std::size_t N>
    struct parser;

    struct option {
        /// Short-option with the form -a -b -c
        /// \param shortopt Character to use as a short option
        consteval explicit option(char shortopt) noexcept : short_name{shortopt} {}
        /// Long-option with the form --hello --long-option
        /// \param longopt String to use as a long option
        consteval explicit option(std::string_view longopt) noexcept : long_name{longopt} {}
        /// Short-option with long-option variant
        /// \param shortopt Character to use as a short option
        /// \param longopt String to use as a long option
        consteval option(char shortopt, std::string_view longopt) noexcept : short_name{shortopt}, long_name{longopt} {}

        /// Minimum number of values
        /// \param _ Non-negative integer
        /// \return Next builder
        [[nodiscard]]
        consteval option min(std::integral auto _) const noexcept {
            auto builder = option{*this};
            builder.m_min = _;
            return builder;
        }

        /// Maximum number of values
        /// \param _ Non-negative integer
        /// \return Next builder
        [[nodiscard]]
        consteval option max(std::integral auto _) const noexcept {
            auto builder = option{*this};
            builder.m_max = _;
            return builder;
        }

        /// This option will increment a flag counter when it appears
        /// \param _ Boolean
        /// \return Next builder
        [[nodiscard]]
        consteval option flag_counter(bool _ = true) const noexcept {
            auto builder = option{*this};
            builder.m_flagCounter = _;
            return builder;
        }

        /// Help-text to be displayed for this option
        /// \param _ String
        /// \return Next builder
        [[nodiscard]]
        consteval option help_text(detail::String auto _) const noexcept {
            auto builder = option{*this};
            builder.m_helpText = _;
            return builder;
        }

        /// The meta value is used as a placeholder for the help-text
        /// \param _ String
        /// \return Next builder
        [[nodiscard]]
        consteval option meta(detail::String auto _) const noexcept {
            auto builder = option{*this};
            builder.m_meta = _;
            return builder;
        }

        /// Default value for option
        /// \param _ String
        /// \return Next builder
        [[nodiscard]]
        consteval option default_value(detail::String auto _) const noexcept {
            auto builder = option{*this};
            builder.m_defaultValue = _;
            return builder;
        }

        /// Character to be used for splitting a value into multiple values
        /// \param _ Splitting character
        /// \return Next builder
        [[nodiscard]]
        consteval option separator(char _) const noexcept {
            auto builder = option{*this};
            builder.m_separator = _;
            return builder;
        }

        /// Required options will error if missing
        /// \param _ Boolean
        /// \return Next builder
        [[nodiscard]]
        consteval option required(bool _ = true) const noexcept {
            auto builder = option{*this};
            builder.m_required = _;
            return builder;
        }

        const char short_name{};
        const std::string_view long_name{};
    private:
        friend struct args;

        template <bool NoError, std::size_t N>
        friend struct parser;

        template <bool NoError, std::size_t N>
        friend struct detail::parse_context;

        [[nodiscard]]
        auto name_pair() const noexcept {
            return detail::name_pair{short_name, long_name};
        }

        std::size_t m_min{0};
        std::size_t m_max{1};
        std::string_view m_helpText{};
        std::string_view m_defaultValue{};
        std::string_view m_meta{};
        char m_separator{};
        bool m_flagCounter{};
        bool m_required{};
    };

    template <bool NoError, std::size_t N>
    struct parser;

    struct args {
        using size_type = std::size_t;

        struct const_iterator {
        private:
            friend args;

            using vector_type = std::vector<std::string>;
        public:
            template <typename T>
            [[nodiscard]]
            auto value() const noexcept -> T {
                if constexpr (std::is_pointer_v<T>) {
                    using element_type = std::remove_cvref_t<typename std::pointer_traits<T>::element_type>;

                    const auto& value = *m_iter;
                    if constexpr (std::is_same_v<element_type, char>) {
                        return value.c_str();
                    } else if constexpr (std::is_same_v<element_type, std::string_view>) {
                        return &value;
                    } else {
                        throw std::invalid_argument("Pointer type not returnable");
                    }
                } else {
                    const auto& value = *m_iter;
                    if constexpr (std::constructible_from<std::string_view, T>) {
                        return value;
                    } else {
                        return from_string<T>(value);
                    }
                }
            }

            auto operator*() const noexcept {
                return *m_iter;
            }

            [[nodiscard]]
            bool operator ==(const const_iterator& rhs) const noexcept {
                return m_iter == rhs.m_iter;
            }

            [[nodiscard]]
            bool operator !=(const const_iterator& rhs) const noexcept {
                return m_iter != rhs.m_iter;
            }

            auto& operator ++() noexcept {
                ++m_iter;
                return *this;
            }

            auto operator ++(int) noexcept {
                return const_iterator(*m_args, m_iter++);
            }

            auto& operator +=(vector_type::difference_type i) noexcept {
                m_iter += i;
                return *this;
            }

        private:
            const_iterator(const vector_type& args, vector_type::const_iterator iter) noexcept : m_args{&args}, m_iter{iter} {}

            const vector_type* m_args;
            vector_type::const_iterator m_iter;
        };

        using iterator = const_iterator;

        /// Retrieve option value
        /// \tparam T type to parse option into
        /// \param _ Short-opt key (char)
        /// \return T
        template <typename T>
        auto get(char shortName) const noexcept {
            return get_if<T>([shortName](const auto& keyValues) {
                return std::get<0>(keyValues).short_name == shortName;
            });
        }

        /// Retrieve option value
        /// \tparam T type to parse option into
        /// \param _ Long-opt key (string)
        /// \return T
        template <typename T>
        auto get(std::string_view longName) const noexcept {
            return get_if<T>([longName](const auto& keyValues) {
                return std::get<0>(keyValues).long_name == longName;
            });
        }

        /// Test if args has an error
        /// \return false if there is an error
        [[nodiscard]]
        explicit operator bool() const noexcept {
            return std::holds_alternative<data_type>(m_data);
        }

        /// Error string (use operator bool() first to check for an error)
        /// \return Error string, if there is an error
        [[nodiscard]]
        const auto& error_str() const noexcept {
            return std::get<std::string>(m_data);
        }

        /// Access positional argument
        /// \tparam T type to parse argument into
        /// \param _ Index into positional arguments
        /// \return Postional argument value
        template <typename T>
        [[nodiscard]]
        auto at(std::integral auto _) const noexcept {
            return positional().at(_);
        }

        /// Number of positional arguments
        /// \return Number
        [[nodiscard]]
        auto size() const noexcept {
            return positional().size();
        }

        /// Const iterator to the beginning of positional values
        /// \return iterator
        [[nodiscard]]
        const_iterator cbegin() const noexcept {
            return const_iterator{positional(), positional().cbegin()};
        }

        /// Const iterator to the end of positional values
        /// \return iterator
        [[nodiscard]]
        const_iterator cend() const noexcept {
            return const_iterator{positional(), positional().cend()};
        }

        /// Iterator to the beginning of positional values
        /// \return iterator
        [[nodiscard]]
        iterator begin() const noexcept {
            return iterator{positional(), positional().begin()};
        }

        /// Iterator to the end of positional values
        /// \return iterator
        [[nodiscard]]
        iterator end() const noexcept {
            return iterator{positional(), positional().end()};
        }

        /// Return a transforming iterator that returns value as T
        /// \tparam T type to make_key_values to
        /// \return iterator
        template <typename T>
        [[nodiscard]]
        auto transform() const noexcept {
            return transform_iterator<T>(this);
        }

        template <typename T>
        struct transform_iterator {
            using value_type = T;
            using difference_type = std::vector<std::string>::difference_type;

            struct const_iterator {
            public:
                auto operator*() const noexcept {
                    return m_iter.value<value_type>();
                }

                [[nodiscard]]
                bool operator ==(const const_iterator& rhs) const noexcept {
                    return m_iter == rhs.m_iter;
                }

                [[nodiscard]]
                bool operator !=(const const_iterator& rhs) const noexcept {
                    return m_iter != rhs.m_iter;
                }

                auto& operator ++() noexcept {
                    ++m_iter;
                    return *this;
                }

                auto operator ++(int) noexcept {
                    return const_iterator(*m_args, m_iter++);
                }

                auto& operator +=(difference_type i) noexcept {
                    m_iter += i;
                    return *this;
                }
            private:
                friend transform_iterator;

                const_iterator(const args* args, args::const_iterator iter) noexcept : m_args{args}, m_iter{iter} {}

                const args* m_args;
                args::const_iterator m_iter;
            };

            using iterator = const_iterator;

            [[nodiscard]]
            const_iterator cbegin() const noexcept {
                return const_iterator{m_args, m_args->cbegin()};
            }

            [[nodiscard]]
            const_iterator cend() const noexcept {
                return const_iterator{m_args, m_args->cend()};
            }

            [[nodiscard]]
            iterator begin() const noexcept {
                return iterator{m_args, m_args->cbegin()};
            }

            [[nodiscard]]
            iterator end() const noexcept {
                return iterator{m_args, m_args->cend()};
            }
        private:
            friend args;

            transform_iterator(const args* args) noexcept : m_args{args} {}

            const args* m_args;
        };
    private:
        template <bool NoError, std::size_t N>
        friend struct parser;

        struct data_type {
            const std::map<detail::name_pair, std::vector<std::string>> key_value;
            const std::vector<std::string> positional;
        };

        static auto make_key_values(const std::map<const option*, std::vector<std::string>>& map) noexcept {
            std::map<detail::name_pair, std::vector<std::string>> result;
            for (const auto& [key, values] : map) {
                result[key->name_pair()] = values;
            }
            return result;
        }

        [[nodiscard]]
        auto key_value() const noexcept -> const std::map<detail::name_pair, std::vector<std::string>>& {
            return std::get<data_type>(m_data).key_value;
        }

        [[nodiscard]]
        auto positional() const noexcept -> const std::vector<std::string>& {
            return std::get<data_type>(m_data).positional;
        }

        template <typename T>
        auto get_if(auto predicate) const noexcept {
            const auto& keyValue = key_value();
            auto iter = std::find_if(keyValue.cbegin(), keyValue.cend(), predicate);
            if (iter == keyValue.cend()) {
                return T{};
            }
            return detail::from_args<T>(std::get<1>(*iter));
        }

        template <bool NoError, std::size_t N>
        explicit args(const detail::parse_context<NoError, N>& parseContext) noexcept :
                m_data{data_type{
                        make_key_values(parseContext.m_elementValues),
                        parseContext.m_positionalValues
                }} {}

        explicit args(std::string&& errorStr) noexcept : m_data{errorStr} {}

        std::variant<data_type, std::string> m_data;
    };

    template <bool NoError, std::size_t N>
    struct parser;

    namespace detail {

        template <std::size_t N>
        auto get_long_option(const std::array<option, N>& options, std::string_view key) noexcept -> std::pair<const option*, std::string_view> {
            for (const auto& option : options) {
                const auto keyEnd = key.find_first_not_of(option.long_name);
                if (keyEnd == std::string_view::npos) {
                    // Only key
                    return std::make_pair(&option, std::string_view{});
                }

                if (keyEnd != option.long_name.size()) {
                    continue;
                }

                if (key[keyEnd] == '=') {
                    return std::make_pair(&option, key.substr(keyEnd + 1));
                }
            }

            return std::make_pair(nullptr, std::string_view{});
        }

        template <std::size_t N>
        auto get_short_options(const std::array<option, N>& options, std::string_view key) noexcept -> std::pair<std::vector<const option*>, std::string_view> {
            std::vector<const option*> opts;

            for (std::string_view::size_type ii = 0; ii < key.size(); ++ii) {
                auto iter = std::find_if(options.cbegin(), options.cend(), [k = key[ii]](const auto& option) {
                    return option.short_name == k;
                });

                if (iter == options.cend()) {
                    return std::make_pair(opts, key.substr(ii));
                }

                opts.push_back(std::to_address(iter));
            }

            return std::make_pair(opts, std::string_view{});
        }

        template <bool NoError, std::size_t N>
        struct parse_context {
        private:
            friend struct parser<NoError, N>;
            friend struct ctopt::args;

            parse_context(const std::array<option, N>& options, bool longOnly) noexcept : m_options{options}, m_longOnly{longOnly} {}

            [[nodiscard]]
            bool operator()(std::string_view arg) noexcept {
                [[maybe_unused]]
                const auto originalArg = arg;

                if (m_currentOption) {
                    // Expecting value
                    if (m_currentOption->m_max > 1 && !m_currentOption->m_flagCounter) {
                        auto& values = m_elementValues[m_currentOption];
                        values.emplace_back(arg);
                        if (values.size() == m_currentOption->m_max) {
                            m_currentOption = nullptr;
                        }
                        return true;
                    }
                    return on_arg(arg);
                }

                // Expecting key
                const auto type = std::min(arg.find_first_not_of('-'), std::string_view::size_type{2});

                if (type == 1) {
                    const auto key = arg.substr(1);

                    if (m_longOnly) {
                        // First try parsing a long option
                        std::tie(m_currentOption, arg) = get_long_option(m_options, key);
                    }

                    if (!m_currentOption) {
                        // Parse short option
                        std::vector<const option*> shortOptions;
                        std::tie(shortOptions, arg) = get_short_options(m_options, key);
                        if (!shortOptions.empty()) {
                            m_currentOption = shortOptions.back();
                            shortOptions.pop_back();

                            for (const auto& shortOpt: shortOptions) {
                                on_popcount_arg(shortOpt);
                            }
                        }
                    }
                } else if (type == 2) {
                    // Parse long option
                    std::tie(m_currentOption, arg) = get_long_option(m_options, arg.substr(2));
                } else {
                    // arg is a positional value
                    on_positional_arg(arg);
                    return true;
                }

                if (m_currentOption) {
                    if (m_currentOption->m_flagCounter) {
                        // We can immediately handle flag counters
                        m_counterValues[m_currentOption] += 1;
                        m_currentOption = nullptr;
                    } else if (!arg.empty() && !on_arg(arg)) {
                        return false;
                    }

                    return true;
                }

                if constexpr (NoError) {
                    // don't error for unknown args
                    on_positional_arg(originalArg);
                    return true;
                }

                m_errorString = "Unknown option: " + std::string{arg};
                return false;
            }

            [[nodiscard]]
            bool finish() noexcept {
                if (m_currentOption) {
                    on_popcount_arg(m_currentOption);
                    m_currentOption = nullptr;
                }

                for (const auto& [key, value] : m_counterValues) {
                    m_elementValues[key].push_back(std::to_string(value));
                }

                for (auto& [key, values] : m_elementValues) {
                    if (values.size() < key->m_min) {
                        if (key->m_meta.empty()) {
                            m_errorString = "No arg given for " + key->name_pair().printable();
                        } else {
                            m_errorString = "No " + std::string{key->m_meta} + " given for " + key->name_pair().printable();
                        }
                        return false;
                    }
                    if (values.size() > key->m_max) {
                        m_errorString = "Unknown option for " + key->name_pair().printable() + ": " + values[key->m_max];
                        return false;
                    }

                    values.shrink_to_fit();
                }

                for (const auto& option : m_options) {
                    if (m_elementValues.contains(&option)) {
                        continue;
                    }

                    if (option.m_required) {
                        m_errorString = "Missing required option: " + option.name_pair().printable();
                        return false;
                    }

                    if (!option.m_defaultValue.empty()) {
                        m_currentOption = &option;
                        if (!on_map_arg(option.m_defaultValue)) {
                            m_elementValues.emplace(&option, std::vector<std::string>(1, std::string{option.m_defaultValue}));
                        }
                    }
                }

                m_positionalValues.shrink_to_fit();
                return true;
            }

            [[nodiscard]]
            bool on_arg(std::string_view arg) noexcept {
                if (m_currentOption->m_flagCounter) {
                    // Flag counters don't take arguments
                    m_counterValues[m_currentOption] += 1;
                    on_positional_arg(arg);
                } else if (!on_map_arg(arg)) {
                    return false;
                }

                m_currentOption = nullptr;
                return true;
            }

            void on_popcount_arg(const option* key) noexcept {
                if (key->m_flagCounter) {
                    m_counterValues[key] += 1;
                    return;
                }

                if (m_elementValues.contains(key)) {
                    return;
                }

                m_elementValues[key].emplace_back("true");
            }

            [[nodiscard]]
            bool on_map_arg(std::string_view arg) noexcept {
                if (m_currentOption->m_flagCounter) {
                    m_counterValues[m_currentOption] += 1;
                } else {
                    auto& values = m_elementValues[m_currentOption];

                    if (m_currentOption->m_separator) {
                        // Error if we're doing separators beyond max args
                        std::size_t begin = 0;
                        while (begin < arg.size()) {
                            if (values.size() == m_currentOption->m_max) {
                                m_errorString = "Unknown option for " + m_currentOption->name_pair().printable() + ": " + std::string{arg.substr(begin)};
                                return false;
                            }

                            auto end = arg.find_first_of(m_currentOption->m_separator, begin);
                            if (end == std::string_view::npos) {
                                values.emplace_back(arg.substr(begin));
                                break;
                            } else {
                                values.emplace_back(arg.substr(begin, end - begin));
                                begin = end + 1;
                            }
                        }
                    } else {
                        values.emplace_back(arg);
                    }
                }

                return true;
            }

            void on_positional_arg(std::string_view arg) noexcept {
                m_positionalValues.emplace_back(arg);
            }

            auto& error_str() noexcept {
                return m_errorString;
            }

            const std::array<option, N>& m_options;
            const bool m_longOnly;

            const option* m_currentOption{nullptr};
            std::map<const option*, std::size_t> m_counterValues {};
            std::map<const option*, std::vector<std::string>> m_elementValues {};
            std::vector<std::string> m_positionalValues {};
            std::string m_errorString;
        };

    } // namespace detail

    template <bool NoError, std::size_t N>
    struct parser {
        /// Construct parser with options
        /// \tparam Opts option type
        /// \param _ variadic list of options
        template <typename... Opts>
        consteval explicit parser(Opts... _) noexcept : m_options{std::forward<Opts>(_)...} {}

        /// Parse standard main arguments
        /// \param argc Argument count
        /// \param argv Pointer to argument vector
        /// \return args type
        [[nodiscard]]
        args operator()(int argc, char* argv[]) const noexcept {
            detail::parse_context<NoError, N> ctx{m_options, m_longOnly};

            for (int ii = 1; ii < argc; ++ii) {
                const auto arg = std::string_view{argv[ii]};
                if (arg == "--") {
                    break; // Stop parsing
                }

                const auto isGood = ctx(arg);
                if (!isGood) {
                    return args{std::move(ctx.error_str())};
                }
            }

            if (!ctx.finish()) {
                return args{std::move(ctx.error_str())};
            }

            return args{ctx};
        }

        /// Parse positional arguments
        /// \param begin Argument container begin
        /// \param end Argument container end
        /// \return args type
        [[nodiscard]]
        args operator()(args::const_iterator begin, args::const_iterator end) const noexcept {
            detail::parse_context<NoError, N> ctx{m_options, m_longOnly};

            while (begin != end) {
                const auto isGood = ctx(*begin);
                if (!isGood) {
                    return args{std::move(ctx.error_str())};
                }
                ++begin;
            }

            if (!ctx.finish()) {
                return args{std::move(ctx.error_str())};
            }

            return args{ctx};
        }

        /// Set parse to first parse short keys as long arguments, before parsing them as short arguments
        /// \param _ bool
        /// \return Next builder
        [[nodiscard]]
        consteval auto long_only(bool _ = true) const noexcept {
            auto next = *this;
            next.m_longOnly = _;
            return next;
        }

        /// Build the GNU style help-text for this option parser
        /// \return std::string containing the help-text
        [[nodiscard]]
        auto help_str() const noexcept {
            auto columnWidth = 0;
            for (const auto& option : m_options) {
                auto width = 2;

                if (option.short_name && !option.long_name.empty()) {
                    width += 5 + option.long_name.size();
                } else if (option.short_name) {
                    width += 2;
                } else if (!option.long_name.empty()) {
                    width += 2 + option.long_name.size();
                }

                if (!option.m_meta.empty()) {
                    width += 1 + option.m_meta.size();
                }

                columnWidth = std::max(columnWidth, width);
            }
            // Convert to 4 character soft-tabs
            columnWidth = ((columnWidth + 3) / 2) * 2;

            auto oss = std::ostringstream{};
            oss << "Options:\n";

            for (const auto& option : m_options) {
                const auto begin = oss.tellp();
                oss << "  ";

                if (option.short_name && !option.long_name.empty()) {
                    oss << "-" << option.short_name << " --" << option.long_name;
                } else if (option.short_name) {
                    oss << "-" << option.short_name;
                } else if (!option.long_name.empty()) {
                    oss << "--" << option.long_name;
                }

                if (!option.m_meta.empty()) {
                    oss << '=' << option.m_meta;
                }

                const auto end = oss.tellp();
                for (auto tab = (end - begin); tab < columnWidth; ++tab) {
                    oss << ' ';
                }

                if (!option.m_helpText.empty() && !option.m_defaultValue.empty()) {
                    oss << option.m_helpText << " [default: " << option.m_defaultValue << "]";
                } else if (!option.m_helpText.empty()) {
                    oss << option.m_helpText;
                } else if (!option.m_defaultValue.empty()) {
                    oss << "[default: " << option.m_defaultValue << "]";
                }

                oss << '\n';
            }
            return oss.str();
        }
    private:
        std::array<option, N> m_options;
        bool m_longOnly{};
    };

    /// Construct parser
    /// \tparam Opts option type
    /// \param _ Variadic list of options
    /// \return parser type
    template <typename... Opts>
    consteval auto make_options(Opts... _) noexcept {
        return parser<false, sizeof...(Opts)>{std::forward<Opts>(_)...};
    }

    /// Construct parser that retains unparsed args
    /// \tparam Opts option type
    /// \param _ Variadic list of options
    /// \return parser type
    template <typename... Opts>
    consteval auto make_options_no_error(Opts... _) noexcept {
        return parser<true, sizeof...(Opts)>{std::forward<Opts>(_)...};
    }

} // namespace ctopt

#endif // define CTOPT_HPP
