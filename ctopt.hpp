/**
 * Crappy C++20 options parser
 *
 * MIT License
 *
 * Copyright (c) 2022 Felix Jones
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
 *         const auto help_text = args.help_str();
 *         // ...
 *     }
 *
 *     const auto verbose = args.get<bool>("verbose");
 *     if (verbose.value_or(false)) {
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
#include <charconv>
#include <cstddef>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <string_view>
#include <utility>
#include <vector>

namespace ctopt {

    struct option;

    namespace detail {

        template <typename T, typename To>
        concept ConstructibleTo = std::constructible_from<To, T>;

        template <typename T>
        concept ArgKey = ConstructibleTo<T, std::string_view> || std::same_as<T, char>;

        template <typename T>
        concept Container = requires(T a, const T b) {
            requires std::regular<T>;
            requires std::swappable<T>;
            requires std::destructible<typename T::value_type>;
            requires std::same_as<typename T::reference, typename T::value_type &>;
            requires std::same_as<typename T::const_reference, const typename T::value_type &>;
            requires std::forward_iterator<typename T::iterator>;
            requires std::forward_iterator<typename T::const_iterator>;
            requires std::signed_integral<typename T::difference_type>;
            requires std::same_as<typename T::difference_type, typename std::iterator_traits<typename T::iterator>::difference_type>;
            requires std::same_as<typename T::difference_type, typename std::iterator_traits<typename T::const_iterator>::difference_type>;
            { std::begin(a) } -> std::same_as<typename T::iterator>;
            { a.end() } -> std::same_as<typename T::iterator>;
            { b.begin() } -> std::same_as<typename T::const_iterator>;
            { b.end() } -> std::same_as<typename T::const_iterator>;
            { std::cbegin(a) } -> std::same_as<typename T::const_iterator>;
            { a.cbegin() } -> std::same_as<typename T::const_iterator>;
            { a.cend() } -> std::same_as<typename T::const_iterator>;
            { a.size() } -> std::same_as<typename T::size_type>;
            { a.max_size() } -> std::same_as<typename T::size_type>;
            { a.empty() } -> std::same_as<bool>;
        };

        template <typename T>
        concept IsVector = Container<T> && std::same_as<std::vector<typename T::value_type>, T>;

        auto arg_type(std::string_view sv) noexcept {
            if (sv.starts_with("--")) {
                return 2;
            }
            if (sv.starts_with('-')) {
                return 1;
            }
            return 0;
        }

        struct name_pair {
            name_pair(const option& option) noexcept;

            auto operator<=>(const name_pair& rhs) const = default;

            operator std::string_view() const noexcept {
                if (!long_name.empty()) {
                    return long_name;
                }
                return std::string_view{&short_name, 1};
            }

            const char short_name;
            const std::string_view long_name;
        };

        template <std::size_t N>
        struct parse_context {
            struct error {
                int which{};
                std::string_view arg{};
                std::string message{};
            };

            parse_context(const std::array<option, N>& options, bool longOnly) noexcept : m_options{options}, m_longOnly{longOnly} {}

            [[nodiscard]]
            bool operator()(int which, std::string_view arg) noexcept;

            [[nodiscard]]
            auto find_options(int type, std::string_view arg, std::string_view::size_type eq) noexcept -> std::optional<std::vector<const option*>> {
                using vector_type = std::vector<const option*>;

                if (type == 2) {
                    // Match long
                    auto iter = std::find_if(cbegin(m_options), cend(m_options), [sv = arg.substr(2, eq - 2)](const auto& option) {
                        return option.long_name == sv;
                    });

                    if (iter != cend(m_options)) {
                        return vector_type{&*iter};
                    }

                    m_error.arg = arg;
                    return std::nullopt;
                }

                const auto shortopt = arg.substr(1);

                if (m_longOnly) {
                    // Try to match long first
                    auto iter = std::find_if(cbegin(m_options), cend(m_options), [shortopt](const auto& option) {
                        return option.long_name == shortopt;
                    });

                    if (iter != cend(m_options)) {
                        return vector_type{&*iter};
                    }
                }

                auto options = vector_type{};

                for (auto c = cbegin(shortopt); c != cend(shortopt); ++c) {
                    auto iter = std::find_if(cbegin(m_options), cend(m_options), [c = *c](const auto& option) {
                        return option.short_name == c;
                    });

                    if (iter == cend(m_options)) {
                        m_error.arg = std::string_view{c, 1};
                        return std::nullopt;
                    }

                    options.push_back(&*iter);
                }

                return options;
            }

            [[nodiscard]]
            std::string error_str() const noexcept {
                auto oss = std::ostringstream{};
                oss << std::string{m_error.message} + ": " + std::string{m_error.arg};
                if (m_error.which) {
                    oss << " (at " + std::to_string(m_error.which) + ")";
                }
                return oss.str();
            };

            [[nodiscard]]
            bool shrink_to_fit() noexcept {
                for (const auto& [key, value] : flag_counter) {
                    element_values[key].emplace_back(std::to_string(value));
                }
                for (const auto& option : m_options) {
                    if (!option.m_defaultValue.empty() && !element_values.contains(option)) {
                        element_values[option].emplace_back(option.m_defaultValue);
                    }
                    if (option.m_required && !element_values.contains(option)) {
                        m_error.message = std::string{"Missing required option '"} + std::string{name_pair{option}} + '\'';
                        m_error.which = -1;
                        return false;
                    }
                }
                for (auto& [key, value] : element_values) {
                    value.shrink_to_fit();
                }
                positional_values.shrink_to_fit();
                return true;
            }

            std::map<name_pair, std::vector<std::string>> element_values;
            std::map<name_pair, std::size_t> flag_counter;
            std::vector<std::string> positional_values;
        private:
            const std::array<option, N>& m_options;
            const bool m_longOnly{};
            const option* m_currentOption{};
            std::size_t m_pushedValues{};
            error m_error{};
        };

    } // namespace detail

    struct option {
        template <std::size_t N>
        friend struct detail::parse_context;

        template <std::size_t N>
        friend struct parser;

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
        consteval option help_text(detail::ConstructibleTo<std::string_view> auto _) const noexcept {
            auto builder = option{*this};
            builder.m_helpText = _;
            return builder;
        }

        /// The meta value is used as a placeholder for the help-text
        /// \param _ String
        /// \return Next builder
        [[nodiscard]]
        consteval option meta(detail::ConstructibleTo<std::string_view> auto _) const noexcept {
            auto builder = option{*this};
            builder.m_meta = _;
            return builder;
        }

        /// Default value for option
        /// \param _ String
        /// \return Next builder
        [[nodiscard]]
        consteval option default_value(detail::ConstructibleTo<std::string_view> auto _) const noexcept {
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
        std::size_t m_min{0};
        std::size_t m_max{1};
        std::string_view m_helpText{};
        std::string_view m_defaultValue{};
        std::string_view m_meta{};
        char m_separator{};
        bool m_flagCounter{};
        bool m_required{};
    };

    namespace {

        /// Parse string to a boolean value
        /// \tparam T bool
        /// \param sv String
        /// \return bool representation of string
        template <std::same_as<bool> T>
        auto from_string(std::string_view sv) noexcept -> T {
            if (sv.empty()) {
                return true;
            }

            std::string lower;
            std::transform(cbegin(sv), cend(sv), std::back_inserter(lower), [](char c){
                return std::tolower(c);
            });

            if (lower == "on" || lower == "yes" || lower == "true" || lower == "y") {
                return true;
            }

            int value{};
            auto [ptr, ec] { std::from_chars(cbegin(sv), cend(sv), value) };
            if (ptr == cend(sv)) {
                return bool(value);
            }

            return false;
        }

        /// Parse string to a numeric value
        /// \tparam T Number
        /// \param sv String
        /// \return Numeric representation of string
        template <typename T> requires (std::integral<T> || std::floating_point<T>) && (!std::is_same_v<T, bool>)
        auto from_string(std::string_view sv) noexcept -> T {
            if (sv.empty()) {
                return 0;
            }

            T value{};
            auto [ptr, ec] { std::from_chars(cbegin(sv), cend(sv), value) };
            if (ptr == cend(sv)) {
                return value;
            }

            return 0;
        }

    } // namespace

    struct args {
    private:
        template <std::size_t N>
        friend struct parser;
    public:
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

        /// Const iterator to the beginning of positional values
        /// \return iterator
        [[nodiscard]]
        const_iterator cbegin() const noexcept {
            return const_iterator{m_positionalValues, std::cbegin(m_positionalValues)};
        }

        /// Const iterator to the end of positional values
        /// \return iterator
        [[nodiscard]]
        const_iterator cend() const noexcept {
            return const_iterator{m_positionalValues, std::cend(m_positionalValues)};
        }

        /// Iterator to the beginning of positional values
        /// \return iterator
        [[nodiscard]]
        iterator begin() const noexcept {
            return iterator{m_positionalValues, std::cbegin(m_positionalValues)};
        }

        /// Iterator to the end of positional values
        /// \return iterator
        [[nodiscard]]
        iterator end() const noexcept {
            return iterator{m_positionalValues, std::cend(m_positionalValues)};
        }

        /// Test if args has an error
        /// \return false if there is an error
        [[nodiscard]]
        explicit operator bool() const noexcept {
            return m_errorMessage.empty();
        }

        /// Error string
        /// \return Empty string if no error
        [[nodiscard]]
        const auto& error_str() const noexcept {
            return m_errorMessage;
        }

        /// Return a transforming iterator that returns value as T
        /// \tparam T type to transform to
        /// \return iterator
        template <typename T>
        [[nodiscard]]
        auto transform() const noexcept {
            return transform_iterator<T>(this);
        }

        /// Access positional argument
        /// \tparam T type to parse argument into
        /// \param _ Index into positional arguments
        /// \return Postional argument value
        template <typename T>
        [[nodiscard]]
        auto at(std::integral auto _) const noexcept {
            auto iter = cbegin();
            iter += _;
            return iter.value<T>();
        }

        /// Number of positional arguments
        /// \return Number
        [[nodiscard]]
        auto size() const noexcept {
            return m_positionalValues.size();
        }

        /// Retrieve option value
        /// \tparam T type to parse option into
        /// \param _ Either short-opt key (char) or long-opt key (string)
        /// \return std::optional<T> for single values, nullable T* for pointers, Container<T> for standard containers
        template <typename T>
        [[nodiscard]]
        auto get(detail::ArgKey auto _) const noexcept -> std::conditional_t<std::is_pointer_v<T> || (detail::Container<T> && (!std::constructible_from<std::string_view, T>)), T, std::optional<T>> {
            const auto iter = std::find_if(std::cbegin(m_elementValues), std::cend(m_elementValues), [_](const auto& p) {
                const auto& [key, value] {p};
                if constexpr (std::is_same_v<decltype(_), char>) {
                    return key.short_name == _;
                } else {
                    return key.long_name == _;
                }
            });

            if constexpr (detail::Container<T> && !std::constructible_from<std::string_view, T>) {
                if (iter == std::cend(m_elementValues)) {
                    return T{}; // Empty container
                }

                const auto& [key, value] {*iter};
                if constexpr (std::constructible_from<std::string_view, T>) {
                    return T{std::cbegin(value), std::cend(value)};
                }
                if constexpr (detail::IsVector<T>) {
                    T container{};
                    container.reserve(value.size());
                    std::transform(std::cbegin(value), std::cend(value), std::back_inserter(container), [](auto item) {
                        return from_string<typename T::value_type>(item);
                    });
                    return container;
                } else {
                    std::vector<typename T::value_type> container{};
                    container.reserve(value.size());
                    std::transform(std::cbegin(value), std::cend(value), std::back_inserter(container), [](auto item) {
                        return from_string<typename T::value_type>(item);
                    });
                    return T{std::cbegin(container), std::cend(container)};
                }
            } else if constexpr (std::is_pointer_v<T>) {
                if (iter == std::cend(m_elementValues)) {
                    return nullptr;
                }

                using element_type = std::remove_cvref_t<typename std::pointer_traits<T>::element_type>;

                const auto& [key, value] {*iter};
                if constexpr (std::is_same_v<element_type, char>) {
                    return value.at(0).c_str();
                } else if constexpr (std::is_same_v<element_type, std::string>) {
                    return &value.at(0);
                } else {
                    throw std::invalid_argument("Pointer type not returnable");
                }
            } else {
                if (iter == std::cend(m_elementValues)) {
                    return std::nullopt;
                }

                const auto& [key, value] {*iter};
                if constexpr (std::constructible_from<std::string_view, T>) {
                    return T{value.at(0)};
                } else {
                    return from_string<T>(value.at(0));
                }
            }
        }

        template <typename T>
        struct transform_iterator {
            using value_type = T;
            using difference_type = std::vector<std::string>::difference_type;

            struct const_iterator {
            public:
                auto operator*() const noexcept {
                    return m_iter.template value<value_type>();
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
        explicit args(std::string errorMessage) noexcept : m_errorMessage{std::move(errorMessage)} {}

        template <std::size_t N>
        explicit args(const detail::parse_context<N>& ctx) noexcept :
                m_elementValues{ctx.element_values}, m_positionalValues{ctx.positional_values} {}

        const std::string m_errorMessage {};
        const std::map<detail::name_pair, std::vector<std::string>> m_elementValues {};
        const std::vector<std::string> m_positionalValues {};
    };

    template <std::size_t N>
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
            detail::parse_context ctx{m_options, m_longOnly};

            for (int ii = 1; ii < argc; ++ii) {
                const auto arg = std::string_view{argv[ii]};
                if (arg == "--") {
                    break; // Stop parsing
                }

                const auto isGood = ctx(ii, arg);
                if (!isGood) {
                    return args{ctx.error_str()};
                }
            }
            if (!ctx.shrink_to_fit()) {
                return args{ctx.error_str()};
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
        return parser<sizeof...(Opts)>{std::forward<Opts>(_)...};
    }

    detail::name_pair::name_pair(const option& option) noexcept : short_name{option.short_name}, long_name{option.long_name} {}

    template <std::size_t N>
    bool detail::parse_context<N>::operator()(int which, std::string_view arg) noexcept {
        const auto type = detail::arg_type(arg);

        if (type) {
            if (m_currentOption && m_pushedValues < m_currentOption->m_min) {
                m_error.which = which;
                m_error.message = "Expected value";
                return false;
            }

            const auto eq = arg.find('=');
            const auto options = find_options(type, arg, eq);
            if (!options.has_value()) {
                m_error.which = which;
                m_error.message = "Unknown option";
                return false;
            }

            if (type == 2 && eq != std::string_view::npos) {
                if (m_currentOption) {
                    if (m_currentOption->m_flagCounter) {
                        flag_counter[*m_currentOption] += 1;
                    } else {
                        auto iter = element_values.find(*m_currentOption);
                        if (iter == cend(element_values)) {
                            element_values[*m_currentOption].emplace_back();
                        }
                    }
                }

                arg = arg.substr(eq + 1);
                if (arg.empty()) {
                    m_currentOption = nullptr;
                } else {
                    m_currentOption = options->front();
                }
                m_pushedValues = 0;
            } else {
                for (const auto& option: *options) {
                    if (m_currentOption) {
                        if (m_currentOption->m_flagCounter) {
                            flag_counter[*m_currentOption] += 1;
                        } else {
                            auto iter = element_values.find(*m_currentOption);
                            if (iter == cend(element_values)) {
                                element_values[*m_currentOption].emplace_back();
                            }
                        }
                    }

                    m_currentOption = option;
                    m_pushedValues = 0;
                }

                return true;
            }
        }

        if (m_currentOption && m_currentOption->m_flagCounter) {
            m_error.which = which;
            m_error.message = "Option is flag counter";
            return false;
        }

        if (m_currentOption && m_pushedValues == m_currentOption->m_max) {
            // Reached maximum expected values
            positional_values.emplace_back(arg);
            m_currentOption = nullptr;
            m_pushedValues = 0;
        } else if (!m_currentOption) {
            positional_values.emplace_back(arg);
        } else {
            if (m_currentOption && m_currentOption->m_separator && arg.find(m_currentOption->m_separator) != std::string_view::npos) {
                const auto key = name_pair{*m_currentOption};

                std::size_t pos = 0;
                std::size_t end = arg.find_first_of(m_currentOption->m_separator);
                while (true) {
                    if (m_pushedValues == m_currentOption->m_max) {
                        m_error.which = which;
                        m_error.message = "Too many values";
                        return false;
                    }

                    auto innerArg = arg.substr(pos, end - pos);
                    element_values[key].emplace_back(innerArg);

                    if (end == std::string_view::npos) {
                        break;
                    }

                    m_pushedValues += 1;

                    pos = end + 1;
                    end = arg.find_first_of(m_currentOption->m_separator, pos);
                }

                m_currentOption = nullptr;
                m_pushedValues = 0;
            } else {
                element_values[*m_currentOption].emplace_back(arg);
                m_pushedValues += 1;
            }
        }

        return true;
    }

} // namespace ctopt

#endif // define CTOPT_HPP
