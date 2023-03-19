# ctopt.hpp

Crappy header-only C++20 options parser

It's a single header: [Download it](ctopt.hpp), copy it into your project, and then `#include "ctopt.hpp"`.

## What is this?

* Single header file options parser
* Parses [GNU style argument syntax](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
* Prints help-text in the old *NIX style format (a bit like [docopt](http://docopt.org/))
* Requires C++20 (I like `concepts`, sorry C++17 folx)
* MIT License

## Why?

Once upon a time I found a super simple C++ options parser that I quite liked and wanted to use in more projects.    
Unfortunately I had no idea what license it used and I was unable to locate the source of where I originally found it.

## Is it actually crappy?

Probably. The name-space is `ctopt` because I wanted to make as much of it as compile-time as possible (compile-time options -> ctopt), without sacrificing simple usage syntax.

Unfortuately, C++ strings aren't quite there yet for compile-time work (as of this writing 2022-10-06), so I instead rushed ahead and wrote a load of crappy code just to get this finished.

## Show me the examples

### Simple verbose flag

This will work with both `-v`, `--verbose`, `--verbose true`, and `--verbose=true` arguments.

```c++
#include "ctopt.hpp"

int main(int argc, char* argv[]) {
  using namespace ctopt;

  // Define options
  static constexpr auto get_opts = make_options(
    option('v', "verbose").help_text("Verbose output")
  );

  // Parse program arguments
  const auto args = get_opts(argc, argv);
  if (!args) {
    const auto error_text = args.error_str();
    const auto help_text = get_opts.help_str();
    // ...
  }

  const auto verbose = args.get<bool>("verbose");
  if (verbose) {
    // Print verbose output
    // ...
  }

  // ...
}
```

### List of integers

This works with the format `--ints 1 2 3`, `--ints 4;5;6`, and `--ints=4;5;6`.

```c++
#include "ctopt.hpp"

int main(int argc, char* argv[]) {
  using namespace ctopt;

  static constexpr auto get_opts = make_options(
    option("ints").help_text("A list of integers").meta("numbers")
      .min(1) // Requires at least 1 value
      .separator(';') // Accepts `;` as a separator
  );

  const auto args = get_opts(argc, argv);
  if (!args) {
    // Handle error/help stuff
    // ...
  }

  const auto ints = args.get<std::list<int>>("ints");
  for (auto i : ints) {
    // Print the integer
    // ...
  }

  // ...
}
```

### Positional arguments

Positional arguments are any values that appear when not expecting an option value.

For example: `--process=true my_file.txt --verbose true output.png` both `my_file.txt` and `output.png` would be positional arguments.

```c++
#include "ctopt.hpp"

int main(int argc, char* argv[]) {
  using namespace ctopt;

  static constexpr auto get_opts = make_options(); // Only expecting positional arguments

  const auto args = get_opts(argc, argv);

  for (auto str : args) {
    // Iterate the string arguments
  }

  for (auto ii = 0u; ii < args.size(); ++ii) {
    // Iterate index style
    auto num = args.at<int>(ii);
  }

  for (auto num : args.transform<int>()) {
    // Iterate each value as an integer
  }
  
  // ...
}
```

## Can I parse into my own type?

If you define a `ctopt::from_string` overload above the header:

```c++
struct my_type {
  std::string_view sv;
};

namespace ctopt {

  template <std::same_as<my_type> T>
  auto from_string(std::string_view sv) noexcept -> T {
    // Do some cool parsing thing here
    return my_type{sv};
  }

}

#include "ctopt.hpp"

// Rest of program
```

Perhaps put your custom types and parsers into their own header and include them first.

## This really is crappy...

I did warn you.

However, **if you want to make improvements and fix stuff then absolutely go ahead**.

PRs and contributions are more than welcome (hey, maybe together we can make this not-crappy).

## I saw an [Oxford comma](https://en.wikipedia.org/wiki/Serial_comma) in this README.md!

It's a habit I picked up from writing Minecraft Java Edition changelogs. Send your complaints to me on [Twitter](https://twitter.com/Xilefian).
