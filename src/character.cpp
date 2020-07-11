#include <meevax/kernel/character.hpp>

namespace meevax::kernel
{
  const std::unordered_map<std::string, object> characters
  {
    {"null",                      make<character>(u8"\u0000", "null"                     )},
    {"start-of-header",           make<character>(u8"\u0001", "start-of-header"          )},
    {"start-of-text",             make<character>(u8"\u0002", "start-of-text"            )},
    {"end-of-text",               make<character>(u8"\u0003", "end-of-text"              )},
    {"end-of-transmission",       make<character>(u8"\u0004", "end-of-transmission"      )},
    {"enquiry",                   make<character>(u8"\u0005", "enquiry"                  )},
    {"acknowledge",               make<character>(u8"\u0006", "acknowledge"              )},
    {"bell",                      make<character>(u8"\u0007", "bell"                     )}, // XXX R7RS requires this as name "alarm"
    {"backspace",                 make<character>(u8"\u0008", "backspace"                )},
    {"horizontal-tabulation",     make<character>(u8"\u0009", "horizontal-tabulation"    )},
    {"line-feed",                 make<character>(u8"\u000A", "line-feed"                )},
    {"vertical-tabulation",       make<character>(u8"\u000B", "vertical-tabulation"      )},
    {"form-feed",                 make<character>(u8"\u000C", "form-feed"                )},
    {"carriage-return",           make<character>(u8"\u000D", "carriage-return"          )},
    {"shift-out",                 make<character>(u8"\u000E", "shift-out"                )},
    {"shift-in",                  make<character>(u8"\u000F", "shift-in"                 )},

    {"data-link-escape",          make<character>(u8"\u0010", "data-link-escape"         )},
    {"device-control-1",          make<character>(u8"\u0011", "device-control-1"         )},
    {"device-control-2",          make<character>(u8"\u0012", "device-control-2"         )},
    {"device-control-3",          make<character>(u8"\u0013", "device-control-3"         )},
    {"device-control-4",          make<character>(u8"\u0014", "device-control-4"         )},
    {"negative-acknowledge",      make<character>(u8"\u0015", "negative-acknowledge"     )},
    {"synchronous-idle",          make<character>(u8"\u0016", "synchronous-idle"         )},
    {"end-of-transmission-block", make<character>(u8"\u0017", "end-of-transmission-block")},
    {"cancel",                    make<character>(u8"\u0018", "cancel"                   )},
    {"end-of-medium",             make<character>(u8"\u0019", "end-of-medium"            )},
    {"substitute",                make<character>(u8"\u001A", "substitute"               )},
    {"escape",                    make<character>(u8"\u001B", "escape"                   )},
    {"file-separator",            make<character>(u8"\u001C", "file-separator"           )},
    {"group-separator",           make<character>(u8"\u001D", "group-separator"          )},
    {"record-separator",          make<character>(u8"\u001E", "record-separator"         )},
    {"unit-separator",            make<character>(u8"\u001F", "unit-separator"           )},

    {"space",                     make<character>(u8"\u0020", "space"                    )},
    {"!",                         make<character>(u8"\u0021"                             )}, // exclamation-mark
    {"\"",                        make<character>(u8"\u0022"                             )}, // quotes
    {"#",                         make<character>(u8"\u0023"                             )}, // hash
    {"$",                         make<character>(u8"\u0024"                             )}, // doller
    {"%",                         make<character>(u8"\u0025"                             )}, // percent
    {"&",                         make<character>(u8"\u0026"                             )}, // ampersand
    {"'",                         make<character>(u8"\u0027"                             )}, // apostrophe
    {"(",                         make<character>(u8"\u0028"                             )}, // open bracket
    {")",                         make<character>(u8"\u0029"                             )}, // close bracket
    {"*",                         make<character>(u8"\u002A"                             )}, // asterisk
    {"+",                         make<character>(u8"\u002B"                             )}, // plus
    {",",                         make<character>(u8"\u002C"                             )}, // comma
    {"-",                         make<character>(u8"\u002D"                             )}, // dash
    {".",                         make<character>(u8"\u002E"                             )}, // full-stop
    {"/",                         make<character>(u8"\u002F"                             )}, // slash

    {"0",                         make<character>(u8"\u0030"                             )},
    {"1",                         make<character>(u8"\u0031"                             )},
    {"2",                         make<character>(u8"\u0032"                             )},
    {"3",                         make<character>(u8"\u0033"                             )},
    {"4",                         make<character>(u8"\u0034"                             )},
    {"5",                         make<character>(u8"\u0035"                             )},
    {"6",                         make<character>(u8"\u0036"                             )},
    {"7",                         make<character>(u8"\u0037"                             )},
    {"8",                         make<character>(u8"\u0038"                             )},
    {"9",                         make<character>(u8"\u0039"                             )},
    {":",                         make<character>(u8"\u003A"                             )}, // colon
    {";",                         make<character>(u8"\u003B"                             )}, // semi-colon
    {"<",                         make<character>(u8"\u003C"                             )}, // less-than
    {"=",                         make<character>(u8"\u003D"                             )}, // equals
    {">",                         make<character>(u8"\u003E"                             )}, // greater-than
    {"?",                         make<character>(u8"\u003F"                             )}, // question-mark

    {"@",                         make<character>(u8"\u0040"                             )}, // at
    {"A",                         make<character>(u8"\u0041"                             )},
    {"B",                         make<character>(u8"\u0042"                             )},
    {"C",                         make<character>(u8"\u0043"                             )},
    {"D",                         make<character>(u8"\u0044"                             )},
    {"E",                         make<character>(u8"\u0045"                             )},
    {"F",                         make<character>(u8"\u0046"                             )},
    {"G",                         make<character>(u8"\u0047"                             )},
    {"H",                         make<character>(u8"\u0048"                             )},
    {"I",                         make<character>(u8"\u0049"                             )},
    {"J",                         make<character>(u8"\u004A"                             )},
    {"K",                         make<character>(u8"\u004B"                             )},
    {"L",                         make<character>(u8"\u004C"                             )},
    {"M",                         make<character>(u8"\u004D"                             )},
    {"N",                         make<character>(u8"\u004E"                             )},
    {"O",                         make<character>(u8"\u004F"                             )},

    {"P",                         make<character>(u8"\u0050"                             )},
    {"Q",                         make<character>(u8"\u0051"                             )},
    {"R",                         make<character>(u8"\u0052"                             )},
    {"S",                         make<character>(u8"\u0053"                             )},
    {"T",                         make<character>(u8"\u0054"                             )},
    {"U",                         make<character>(u8"\u0055"                             )},
    {"V",                         make<character>(u8"\u0056"                             )},
    {"W",                         make<character>(u8"\u0057"                             )},
    {"X",                         make<character>(u8"\u0058"                             )},
    {"Y",                         make<character>(u8"\u0059"                             )},
    {"Z",                         make<character>(u8"\u005A"                             )},
    {"[",                         make<character>(u8"\u005B"                             )}, // open-square-bracket
    {"\\",                        make<character>(u8"\u005C"                             )}, // backslash
    {"]",                         make<character>(u8"\u005D"                             )}, // close-square-bracket
    {"^",                         make<character>(u8"\u005E"                             )}, // caret / hat
    {"_",                         make<character>(u8"\u005F"                             )}, // underscore

    {"`",                         make<character>(u8"\u0060"                             )}, // grave-accent
    {"a",                         make<character>(u8"\u0061"                             )},
    {"b",                         make<character>(u8"\u0062"                             )},
    {"c",                         make<character>(u8"\u0063"                             )},
    {"d",                         make<character>(u8"\u0064"                             )},
    {"e",                         make<character>(u8"\u0065"                             )},
    {"f",                         make<character>(u8"\u0066"                             )},
    {"g",                         make<character>(u8"\u0067"                             )},
    {"h",                         make<character>(u8"\u0068"                             )},
    {"i",                         make<character>(u8"\u0069"                             )},
    {"j",                         make<character>(u8"\u006A"                             )},
    {"k",                         make<character>(u8"\u006B"                             )},
    {"l",                         make<character>(u8"\u006C"                             )},
    {"m",                         make<character>(u8"\u006D"                             )},
    {"n",                         make<character>(u8"\u006E"                             )},
    {"o",                         make<character>(u8"\u006F"                             )},

    {"p",                         make<character>(u8"\u0070"                             )},
    {"q",                         make<character>(u8"\u0071"                             )},
    {"r",                         make<character>(u8"\u0072"                             )},
    {"s",                         make<character>(u8"\u0073"                             )},
    {"t",                         make<character>(u8"\u0074"                             )},
    {"u",                         make<character>(u8"\u0075"                             )},
    {"v",                         make<character>(u8"\u0076"                             )},
    {"w",                         make<character>(u8"\u0077"                             )},
    {"x",                         make<character>(u8"\u0078"                             )},
    {"y",                         make<character>(u8"\u0079"                             )},
    {"z",                         make<character>(u8"\u007A"                             )},
    {"{",                         make<character>(u8"\u007B"                             )}, // open-brace
    {"|",                         make<character>(u8"\u007C"                             )}, // pipe
    {"}",                         make<character>(u8"\u007D"                             )}, // close-brace
    {"~",                         make<character>(u8"\u007E"                             )}, // tilde
    {"delete",                    make<character>(u8"\u007F", "delete"                   )},
  }; // characters
} // namespace meevax::kernel
