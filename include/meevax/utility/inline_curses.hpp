#ifndef INCLUDED_MEEVAX_UTILITY_INLINE_CURSES_HPP
#define INCLUDED_MEEVAX_UTILITY_INLINE_CURSES_HPP

#include <list>
#include <string>
#include <system_error>
#include <utility>

#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

namespace meevax::utility
{
  // 割りきってそれなりに都合の良い状況を想定して手を抜く
  class inline_curses
    : public std::list<std::string>
  {
    class termios
      : public ::termios
    {
      const ::termios default_;

    public:
      termios()
        : default_ {(::tcgetattr(STDIN_FILENO, this), *this)}
      {
        c_lflag &= ~(ICANON | ECHO);
        c_cc[VMIN] = 1;
        c_cc[VTIME] = 0;

        if (::tcsetattr(STDIN_FILENO, TCSANOW, this) < 0)
        {
          throw std::system_error {errno, std::system_category()};
        }
      }

      ~termios()
      {
        ::tcsetattr(STDIN_FILENO, TCSANOW, this);
      }
    } termios;

    struct window
      : public ::winsize
    {
      unsigned short& row {ws_row};
      unsigned short& column {ws_col};

      void update() noexcept
      {
        ::ioctl(STDIN_FILENO, TIOCGWINSZ, this);
      }
    } window;

    // テキスト左上からカーソルまでの距離
    struct cursor { std::size_t row {0}, column {0}; } cursor;

    // テキスト左上からウィンドウ左上までの距離
    struct scroll { std::size_t row {0}, column {0}; } scroll;

  public:
    inline_curses()
      : std::list<std::string> {""}
    {
      window.update();
      write();
    }

    void read()
    {
      std::string s {static_cast<char>(std::getchar())};

      static const std::regex escseq {"^\\\e\\[(\\d*;?)+(.|~)$"};

      auto [row, column] {position()};

      switch (s[0])
      {
      case '\e':
        while (!std::regex_match(s, escseq))
        {
          s.push_back(std::getchar());
        }

             if (s == "\e[A") up();
        else if (s == "\e[B") down();
        else if (s == "\e[C") forward();
        else if (s == "\e[D") backward();

        break;

      case '\n':
        // 見た目と処理内容がちぐはぐな点に注意
        emplace(row, std::begin(*row), column);
        row->erase(std::begin(*row), column);

        // ウィンドウ内で実行された改行はスクロールを伴わない
        if (cursor.row + 1 < window.row)
        {
          ++cursor.row;
          cursor.column = std::size(*row);
        }

        // ウィンドウ最下行で実行された改行はウィンドウを押し下げる
        else
        {
          ++scroll.row;
        }

        break;

      default:
        ++cursor.column;
        row->insert(column, std::begin(s), std::end(s));

        break;
      }
    }

    void write()
    {
      static auto wrote {0};

      for (; wrote; --wrote)
      {
        std::cout << "\e[A";
      }

      for (auto line {std::next(std::begin(*this), scroll.row)};
           line != std::end(*this) && wrote < window.row;
           ++line, ++wrote)
      {
        std::cout << "\r\e[K" << scroll.row + wrote << " " << *line << "\n";
      }

      // ステータスライン
      std::cout << "cursor[" << cursor.row << ", " << cursor.column << "] ";
      std::cout << "scroll[" << scroll.row << ", " << scroll.column << "] ";
      std::cout << "window[" << window.row << ", " << window.column << "] ";
      std::cout << "size: " << std::size(*this);

      // std::cout << "\e[" << "A";
      // std::cout << "\e[" << std::next(std::begin(*this), scroll.row)->size() << "C";
    }

  protected:
    auto position() noexcept
      -> std::pair<
           std::list<std::string>::iterator,
           std::string::iterator
         >
    {
      auto row {std::next(std::begin(*this), cursor.row)};
      auto column {std::next(std::begin(*row), cursor.column)};

      return std::make_pair(row, column);
    }

    void up()
    {
      // ウィンドウの一番上では無効
      if (cursor.row)
      {
        --cursor.row;
      }

      if (cursor.row < scroll.row)
      {
        --scroll.row;
      }
    }

    void down()
    {
      if (cursor.row + 1 < std::size(*this))
      {
        ++cursor.row;
      }

      // ここで引いてる１は最終行のメッセージ行
      if (window.row + scroll.row - 1 <= cursor.row)
      {
        ++scroll.row;
      }
    }

    void forward()
    {
      auto [row, column] {position()};

      if (cursor.column < std::size(*row))
      {
        ++cursor.column;
      }
    }

    void backward()
    {
      if (cursor.column)
      {
        --cursor.column;
      }
    }
  };
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_INLINE_CURSES_HPP

