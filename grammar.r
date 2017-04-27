REBOL [
   Title: "Rebol 3 parser"
   Type: 'module
   Exports: [scan-source]
]


debug: :print

err: _
pos: _
line-no: 1
last-line: _
open-at: 0x0

read-hex: function [
    num [string! binary!]
][
    num: uppercase num
    ret: 0
    for-each d num [
        case [
            all [d >= #"0" d <= #"9"][
                ret: ret * 16 + (to integer! d) - 48 ; 48 = to integer! #"0"
            ]
            all [d >= #"A" d <= #"F"][
                ret: ret * 16 + (to integer! d) - 65 + 10; 65 = to integer! #"A"
            ]
            'else [
                fail spaced ["Invalid hex digit:" d]
            ]
        ]
    ]
    ret
]

syntax-errors: [
    invalid-integer             "Invalid integer"
    invalid-time                "Invalid time"
    invalid-tuple               "Invalid tuple"
    invalid-issue               "Invalid issue"
    invalid-path                "Invalid path"
    invalid-refinement          "Invalid refinement"
    invalid-char                "Invalid char"
    invalid-hex-digit           "Invalid hex digit"
    invalid-lit-word-path       "Invalid lit word or path"
    invalid-get-word-path       "Invalid get word or path"
    odd-binary-digit            "Dangling digit at the end" ;must be in pair
    missing-close-paren         "Missing a close parenthesis ')'"
    missing-close-brace         "Missing a close brace '}'"
    missing-close-bracket       "Missing a close bracket ']'"
    missing-close-quote         {Missing a quotation mark (")}
    unrecognized-named-escape   "Unrecognized named escape"
    mal-construct               "Malconstruct"
    syntax-error                "Syntax error"
]

abort: func [
    reason [word!]
][
    print ["Aborting due to" reason "at:" line-no "x" (index-of pos) - (index-of last-line)]
    print ["Last open-at:" mold open-at]
    print ["source:" to string! pos]
    err: reason
    fail (select* syntax-errors reason else spaced ["Unknown error" reason])
]

digit: charset "0123456789"
hex-digit: charset "0123456789ABCDEF"
letter: charset [#"a" - #"z"]
sign: charset "+-"
byte: complement charset {}
lit-prefix: {'}
space-char: charset "^@^(01)^(02)^(03)^(04)^(05)^(06)^(07)^(08)^(09)^(0A)^(0B)^(0C)^(0D)^(0E)^(0F)^(10)^(11)^(12)^(13)^(14)^(15)^(16)^(17)^(18)^(19)^(1A)^(1B)^(1C)^(1D)^(1E)^(1F)^(20)^(7F)"
non-space: complement space-char
regular-word-char: charset ["!&*=?" #"A" - #"Z" #"_" "^`" #"a" - #"z" #"|" #"~"
    #"^(80)" - #"^(BF)" ;old control chars and alternate chars
    #"^(C2)" - #"^(FE)"
] ;can appear in anywhere in a word

special-char: charset "@%\:'<>+-~|_.,#$"

non-quote: complement charset "^""
non-close-brace: complement charset "^}"

not-counted-line-break: [
    "^(0A)^(0D)"
    | #"^(0A)"
    | #"^(0D)"
]

line-break: [
    not-counted-line-break
    last-line: (++ line-no)
]

space: [
    line-break
    | space-char
]

non-space-delimiter: charset {()[]{}^"/;}
delimiter: [
    space
    | non-space-delimiter
    | end
]

and-delimiter: [
    [ and
        [
            not-counted-line-break
            | space-char
            | non-space-delimiter
            | end
        ]
    ]
]

open-brace: [
    pos: #"^{" (open-at: to pair! reduce [line-no 1 + (index-of pos) - (index-of last-line)])
]

open-bracket: [
    pos: #"[" (open-at: to pair! reduce [line-no 1 + (index-of pos) - (index-of last-line)])
]

open-quote: [
    pos: #"^"" (open-at: to pair! reduce [line-no 1 + (index-of pos) - (index-of last-line)])
]

open-paren: [
    pos: #"(" (open-at: to pair! reduce [line-no 1 + (index-of pos) - (index-of last-line)])
]

required-close-brace: [
    #"^}" | pos: (abort 'missing-close-brace)
]

required-quote: [
    #"^"" | pos: (abort 'missing-close-quote)
]

required-close-paren: [
    [#")" | pos: (abort 'missing-close-paren)]
]

required-close-bracket: [
    [#"]" | pos: (abort 'missing-close-bracket)]
]

required-close-angle: [
]

unsigned-integer: context [
    val: 0
    s: _

    rule: [
        copy s [
            some [digit | #"'"] ;allow leading zeros
        ]
        (if error? err: try [val: to integer! s][
            ;FIXME: examine the error for better error message
            abort 'invalid-integer
        ])
    ]
]

integer: context [
    val: 0
    s: _

    rule: [
        copy s [
            opt sign
            unsigned-integer/rule
        ]
        (if error? err: try [val: to integer! s][
            ;FIXME: examine the error for better error message
            abort 'invalid-integer
        ])
    ]
]

unsigned-decimal: context [
    fraction: [
        [#"," | #"."]
        any digit
    ]

    exponential: [
        #"E" integer/rule
    ]

    rule: [
        opt [
            digit
            any [digit | #"'"]
        ]
        [
            [fraction opt exponential]
            | [opt fraction exponential]
        ]
    ]
]

decimal: context [
    val: _
    s: _
    rule: [
        copy s [
            opt sign
            unsigned-decimal/rule
        ]
        (val: to decimal! s)
    ]
]

any-number: context [
    val: _
    rule: [
        decimal/rule (val: decimal/val)
        | integer/rule (val: integer/val)
    ]
]

percent: context [
    val: 0%

    rule: [
        any-number/rule #"%" (val: to percent! any-number/val / 100)
    ]
]

money: context [
    val: $0.00
    s: _
    sign-char: _

    rule: [
        opt copy sign-char sign ;-$2.1, not $-2.1
        #"$"
        copy s unsigned-decimal/rule
        (
            val: to money! to decimal! either blank? sign [s][join-of sign-char s]
        )
    ]
]

time: context [
    val: 00:00:00
    hour: 0
    minute: 0
    sec: 0.0
    t: _
    intra-day: false

    init: does [
        val: 00:00:00
        hour: 0
        minute: 0
        sec: 0.0
        intra-day: false
    ]

    rule: [
        (init)
        pos:
        opt [integer/rule (hour: integer/val)]
        #":"
        integer/rule (minute: integer/val)
        opt [
            #":" any-number/rule (sec: any-number/val)
            opt [
                [
                     "AM"
                     | "PM" (hour: hour + 12)
                ]
                (intra-day: true)
            ]
        ]
        (
            val: make time! reduce [hour minute sec]
            if all [intra-day any [hour < 0 hour >= 24]][
                abort 'invalid-time
            ]
        )
    ]
]

pair: context [
    val: 0x0
    x: _

    rule: [
        [
            any-number/rule (x: any-number/val)
            #"x"
            any-number/rule
            (val: to pair! reduce [x any-number/val])
        ]
    ]
]

date: context [
    val: _
    day: _
    month: _
    year: _
    tz: _
    t: _
    sep: _
    sign-char: _

    init: does [
        day: _
        month: _
        year: _
        t: _
        tz: _
    ]

    named-month: [
        "Jan" opt [#"u" opt [#"a" opt [ #"r" opt #"y"]]]                            (month: 1)
        | "Feb" opt [#"r" opt [#"u" opt [#"a" opt [ #"r" opt #"y"]]]]               (month: 2)
        | "Mar" opt [#"c" opt opt #"h"]                                             (month: 3)
        | "Apr" opt [#"i" opt opt #"l"]                                             (month: 4)
        | "May"                                                                     (month: 5)
        | "Jun" opt #"e"                                                            (month: 6)
        | "Jul" opt #"y"                                                            (month: 7)
        | "Aug" opt [#"u" opt [#"s" opt #"t"]]                                      (month: 8)
        | "Sep" opt [#"t" opt [#"e" opt [ #"m" opt [#"b" opt [#"e" opt #"r"]]]]]    (month: 9)
        | "Oct" opt [#"o" opt [#"b" opt [#"e" opt #"r"]]]                           (month: 10)
        | "Nov" opt [#"e" opt [ #"m" opt [#"b" opt [#"e" opt #"r"]]]]               (month: 11)
        | "Dec" opt [#"e" opt [ #"m" opt [#"b" opt [#"e" opt #"r"]]]]               (month: 12)
    ]

    rule: [
        (init)
        [
            unsigned-integer/rule (day: unsigned-integer/val)
            set sep [#"-" | #"/"]
            [
                unsigned-integer/rule (month: unsigned-integer/val)
                | named-month
            ]
            sep
            unsigned-integer/rule (year: unsigned-integer/val)
            opt [
                #"/"
                time/rule (t: time/val)
                opt [
                    set sign-char sign
                    time/rule (
                        either sign-char = #"-" [
                            tz: negate time/val
                        ][
                            tz: time/val
                        ]
                    )
                ]
            ]
            (
                val: reduce [day month year]
                if t [append val t]
                if tz [append val tz]
                val: make date! val
            )
        ]
    ]
]

string: context [
    val: _
    s: _
    b: _
    c: _

    named-escapes: [
        "null"     "^(null)"
        "line"     "^(line)"
        "tab"      "^(tab)"
        "page"     "^(page)"
        "escape"   "^(escape)"
        "esc"      "^(esc)"
        "back"     "^(back)"
        "del"      "^(del)"
    ]

    escapes: [
        #"/"        #"^/"
        #"!"        #"^!"
        #"-"        #"^-"
        #"~"        #"^~"
        #"@"        #"^@"
        #"a"        #"^a"
        #"b"        #"^b"
        #"c"        #"^c"
        #"d"        #"^d"
        #"e"        #"^e"
        #"f"        #"^f"
        #"g"        #"^g"
        #"h"        #"^h"
        #"i"        #"^i"
        #"j"        #"^j"
        #"k"        #"^k"
        #"l"        #"^l"
        #"m"        #"^m"
        #"n"        #"^n"
        #"o"        #"^o"
        #"p"        #"^p"
        #"q"        #"^q"
        #"r"        #"^r"
        #"s"        #"^s"
        #"t"        #"^t"
        #"u"        #"^u"
        #"v"        #"^v"
        #"w"        #"^w"
        #"x"        #"^x"
        #"y"        #"^y"
        #"z"        #"^z"
        #"["        #"^["
        #"\"        #"^\"
        #"]"        #"^]"
        #"_"        #"^_"
    ]

    unescape: func [
        src [string!] "modified"
    ][
        parse src [
            while [
                change {^^^^} {^^}
                | change ["^^" open-paren
                    [
                        copy s [ 4 digit | 2 hex-digit] and ")" (c: to char! read-hex s) ;and ")" is to prevent it matches the "ba" in "back"
                        | copy s [some letter] (c: select named-escapes s if blank? c [abort 'unrecognized-named-escape])
                    ]
                    required-close-paren] c
                | change ["^^" set b byte (c: any [select escapes b b])] c
                | skip
            ]
        ]
        src
    ]

    init: does [
        val: _
        b: _
        c: _
    ]
    non-close-brace: complement charset "^}"

    in-brace-rule: [
        any [
            {^^^^}
            | "^^{"
            | "^^}"
            | line-break

            ;unescaped braces
            | open-brace in-brace-rule required-close-brace
            | non-close-brace
        ]
    ]

    rule: [
        (init)
        [
            open-quote copy val [
                any [
                    {^^^^}
                    | {^^"}
                    | line-break (abort 'missing-close-quote)
                    | non-quote
                ]
            ] required-quote
            | open-brace copy val [ in-brace-rule ] required-close-brace
        ](
            ;process escaping
            ;print ["unescapped string:" mold val]
            ;trace on
            unescape val
            ;trace off
            ;print ["escapped string:" mold val]
        )
    ]
]

binary: context [
    val: _
    h1: _
    h2: _
    rule: [
        "#" open-brace (val: make binary! 1)
            any [
                and #"}" break
                | space
                | [
                    [set h1 hex-digit | pos: (abort 'invalid-hex-digit)]
                    opt space
                    pos:
                    [
                        set h2 hex-digit
                        | #"^}" (abort 'odd-binary-digit)
                        | (abort 'invalid-hex-digit)
                    ]
                ] (append val read-hex ajoin [h1 h2])
            ]
        required-close-brace
    ]
]

word: context [
    val: _
    s: _

    num-starter: charset "+-." ;must be followed by a non-digit
    rule: [
        copy s [
            [
                num-starter [and-delimiter | and #":"] ;num-starters can be words by themselves
                | [
                    opt num-starter
                    [regular-word-char | num-starter]
                    any [num-starter | regular-word-char | digit]
                ]
                | some #"/" and-delimiter ;all-slash words
            ]
        ] (val: to word! to string! s)
        ;special words starting with #"<", not a tag
        | "<<" and-delimiter (val: '<<)
        | "<=" and-delimiter (val: '<=)
        | "<>" and-delimiter (val: '<>)
        | "<|" and-delimiter (val: to word! "<|")
        | "<-" and-delimiter (val: to word! "<-")
        | ">>" and-delimiter (val: '>>)
        | ">=" and-delimiter (val: '>=)

        ; some special chars can be signle-char words
        ; "@%\:,',$" are exceptions
        | #"#" and-delimiter (val: _) ;bug???
        | #"<" and-delimiter (val: '<)
        | #">" and-delimiter (val: '>)
    ]
]

get-word: context [
    val: _
    rule: [
        #":"
        [word/rule (val: to get-word! word/val) | pos: (abort 'invalid-get-word-path)]
    ]
]

set-word: context [
    val: _
    rule: [
        word/rule #":" (val: to set-word! word/val)
    ]
]

lit-word: context [
    val: _
    rule: [
        lit-prefix
        [
            word/rule (val: to lit-word! word/val)
            | pos: (abort 'invalid-lit-word-path)
        ]
    ]
]

issue: context [
    val: _
    issue-char: charset "',.+-"
    rule: [
        #"#" [
            copy val some [
                ;delimiter reject
                issue-char
                | regular-word-char
                | digit
            ] (val: to issue! val)
            | pos: (abort 'invalid-issue)
        ]
    ]
]

refinement: context [
    val: _
    rule: [
        pos: #"/"
        [
            some #"/" fail ; all slashes are words
            ;exclude some words that can't be refinement
            | pos: [#"<" | #">"] (abort 'invalid-refinement)
            | integer/rule (val: to refinement! integer/val)
            | word/rule (val: to refinement! word/val)
        ]
    ]
]



get-path: context [
    val: _
    rule: [
        #":"
        path/rule (val: to get-path! path/val)
    ]
]

set-path: context [
    val: _
    rule: [
        path/rule #":" (val: to set-path! path/val)
    ]
]

lit-path: context [
    val: _
    rule: [
        lit-prefix
        [
            [lit-prefix | #":"] (abort 'invalid-lit-word-path)
            | path/rule (val: to lit-path! path/val)
        ]
    ]
]

file: context [
    val: _
    s: _
    white-char: charset [#"^(00)" - #"^(20)"]
    valid-in-quotes: complement charset {:;^"}
    valid: complement union white-char charset {:;()[]^"}
    rule: [
        #"%" [
            [
                open-quote
                copy s [any valid-in-quotes]
                required-quote
            ] | [
                copy s [any valid]
            ]
        ]
        (val: to file! s)
    ]
]

stack: make block! 32

group: context [
    val: _
    rule: [
        open-paren
        rebol/rule (val: as group! rebol/val)
        required-close-paren
    ]
]

block: context [
    val: _
    rule: [
        open-bracket
        rebol/rule (val: rebol/val)
        required-close-bracket
    ]
]

tag: context [
    val: _
    non-angle-bracket: complement charset ">"
    rule: [
        #"<"
        copy val [
            some [ ; "<>" is a word!
                [#"^"" any non-quote required-quote] ; ">" can be embedded if it's quoted
                | non-angle-bracket
            ]
        ]
        #">" (val: to tag! val); | pos: (abort 'missing-close-angle-bracket)]
    ]
]

url: context [
    val: _
    s: _
    c: _
    rule: [
        copy val [
            word/rule
            "://"
            any [
                [#"%" 2 hex-digit]
                | #"/"
                | and delimiter break
                | skip
            ]
        ] (
            ; replace all %xx
            parse val [
                while [
                    change [#"%" copy s [2 hex-digit] (c: to char! read-hex s)] c
                    | skip
                ]
            ]

            val: to url! val
        )
    ]
]

char: context [
    val: _
    rule: [
        #"#" [
            #"^"" copy val [
                "^^^^"
                | "^^^""
                | "^^" open-paren some letter required-close-paren ;named escape
                | "^^" skip ;escape
                | non-quote
            ] #"^""
            | #"^{" copy val [
                "^^^^"
                | "^^^""
                | "^^" open-paren some letter required-close-paren ;named escape
                | "^^" skip ;escape
                | non-close-brace
            ] #"^}"
        ] (
            string/unescape val
            unless 1 = length val [
                ;print ["length of string is not 1" mold string/val]
                abort 'invalid-char
            ]
            val: first val
        )
    ]
]

construct: context [
    val: _
    rule: [
        #"#"
        block/rule
        (
            ;debug ["block:" mold block/val]
            switch/default length block/val [
                2 [
                    insert block/val :make
                    val: do bind block/val lib
                ]
                1 [; #[false] #[true] or #[none]
                    val: switch/default first block/val [
                        true [true]
                        false [false]
                        none [_]
                    ][
                        abort 'mal-construct
                    ]
                ]
            ][
                abort 'mal-construct
            ]
        )
    ]
]

byte-integer: context [
    val: _
    four-or-less: charset "01234"
    five-or-less: charset "012345"
    rule: [
        copy val [
            #"1" opt [digit opt digit]
            | #"2" [
                four-or-less digit
                | #"5" five-or-less
            ]
            | digit opt digit
        ] (val: to integer! val)
    ]
]

tuple: context [
    val: _

    rule: [
        (val: make binary! 7)
        opt #"+"
        byte-integer/rule (append val byte-integer/val)
        #"." byte-integer/rule (append val byte-integer/val)
        some [#"."
            [
                byte-integer/rule (append val byte-integer/val)
                | pos: (abort 'invalid-tuple)
            ]
        ]

        (val: to tuple! val)
    ]
]

email: context [
    val: _
    leading-email-char: complement union space-char charset "<@"
    non-at: complement union space-char charset "@"
    rule: [
        copy val [
            leading-email-char
            any non-at
            #"@"
            any [delimiter break | skip] ; "to delimiter" causes invalid-rule error ???
        ] (val: to email! val)
    ]
]

void: context [
    val: _
    rule: [
        #"(" any space #")" (value: ())
    ]
]

comment: context [
    val: _
    rule: [
        copy val [
            #";" any [
                line-break break
                | skip
            ]
        ] ;(print ["comment found:" mold val])
    ]
]

pre-parse: func [
    source [binary! string!]
][
    clear stack
    pos: source
    last-line: source
    line-no: 1
    err: _
]

path: context [
    val: _
    rule: [
        (insert/only stack nested-path/val)
        nested-path/rule (
            val: nested-path/val
            nested-path/val: take stack
        ) | (nested-path/val: take stack) fail
    ]
]

nested-path: context [
    val: _

    rule: [
        word/rule (
            val: make path! 1
            append/only val word/val
        )
        some [
            #"/"
            [
                #"|" and-delimiter              (append/only val '|)
                | "'|" and-delimiter            (append/only val to lit-bar! '|)
                | #"_" and-delimiter            (append/only val _)
                | block/rule                    (append/only val block/val)                ;    [
                ;| void/rule                    (append/only val block/val)                ;    (
                | group/rule                    (append/only val group/val)                ;    (
                | string/rule                   (append/only val string/val)               ;    {
                | char/rule                     (append/only val char/val)                 ;    #{ or #"
                | binary/rule                   (append/only val binary/val)               ;    #{ or 64#
                | construct/rule                (append/only val construct/val)            ;    #[
                | issue/rule                    (append/only val issue/val)                ;    #
                | file/rule                     (append/only val file/val)                 ;    %
                | money/rule                    (append/only val money/val)                ;    $
                ;| lit-path/rule                 (append/only val lit-path/val)             ;    '
                ;| get-path/rule                 (append/only val get-path/val)             ;    :
                | lit-word/rule                 (append/only val lit-word/val)             ;    '
                | get-word/rule                 (append/only val get-word/val)             ;    :

                | email/rule                    (append/only val email/val)

                | pair/rule                     (append/only val pair/val)                 ;before percent
                | tuple/rule                    (append/only val tuple/val)                ;before percent
                | percent/rule                  (append/only val percent/val)              ;before decimal
                | decimal/rule                  (append/only val decimal/val)              ;before integer
                | time/rule                     (append/only val time/val)                 ;before integer
                ;| date/rule                     (append/only val date/val)                 ;before integer
                | integer/rule ;[and-delimiter | (abort 'invalid-integer)]
                                                (append/only val integer/val)
                | url/rule                      (append/only val url/val)                  ;before set-word
                ;| set-path/rule                 (append/only val set-path/val)             ;before path
                ;| path/rule                     (append/only val path/val)                 ;before word
                | set-word/rule                 (append/only val set-word/val)             ;before word
                | word/rule                     (append/only val word/val)                 ;before refinement, because #"/" could be a word

                | refinement/rule               (append/only val refinement/val)           ;#"/"
                | tag/rule                      (append/only val tag/val)                  ;#"<"
                | pos: (abort 'invalid-path)
            ]
        ]
    ]
]

nested-rebol: context [
    val: _
    rule: [
        (val: make block! 1)
        any [
            pos:
            ; sequence is important
            end
            | space
            | comment/rule
            | #"|" and-delimiter            (append/only val '|)
            | "'|" and-delimiter            (append/only val to lit-bar! '|)
            | #"_" and-delimiter            (append/only val _)
            | block/rule                    (append/only val block/val)                ;    [
            ;| void/rule                    (append/only val block/val)                ;    (
            | group/rule                    (append/only val group/val)                ;    (
            | string/rule                   (append/only val string/val)               ;    {
            | char/rule                     (append/only val char/val)                 ;    #{ or #"
            | binary/rule                   (append/only val binary/val)               ;    #{ or 64#
            | construct/rule                (append/only val construct/val)            ;    #[
            | issue/rule                    (append/only val issue/val)                ;    #
            | file/rule                     (append/only val file/val)                 ;    %
            | money/rule                    (append/only val money/val)                ;    $
            | lit-path/rule                 (append/only val lit-path/val)             ;    '
            | get-path/rule                 (append/only val get-path/val)             ;    :
            | lit-word/rule                 (append/only val lit-word/val)             ;    '
            | get-word/rule                 (append/only val get-word/val)             ;    :

            | email/rule                    (append/only val email/val)

            | pair/rule                     (append/only val pair/val)                 ;before percent
            | tuple/rule                    (append/only val tuple/val)                ;before percent
            | percent/rule                  (append/only val percent/val)              ;before decimal
            | decimal/rule                  (append/only val decimal/val)              ;before integer
            | time/rule                     (append/only val time/val)                 ;before integer
            | date/rule                     (append/only val date/val)                 ;before integer
            | integer/rule [and-delimiter | (abort 'invalid-integer)]
                                            (append/only val integer/val)
            | url/rule                      (append/only val url/val)                  ;before set-word
            | set-path/rule                 (append/only val set-path/val)             ;before path
            | path/rule                     (append/only val path/val)                 ;before word
            | set-word/rule                 (append/only val set-word/val)             ;before word
            | word/rule                     (append/only val word/val)                 ;before refinement, because #"/" could be a word

            | refinement/rule               (append/only val refinement/val)           ;#"/"
            | tag/rule                      (append/only val tag/val)                  ;#"<"
            ;| skip                          ();invalid UTF8 byte?
            | [and [#")" | #"]"] | pos: (abort 'syntax-error)]
        ]
    ]
]

rebol: context [
    val: _
    rule: [
        (insert/only stack nested-rebol/val)
        nested-rebol/rule (
            val: nested-rebol/val
            nested-rebol/val: take stack
        ) | (nested-rebol/val: take stack) fail
    ]
]

scan-source: function [
    source [binary! string!]
][
;    debug ["scanning:" mold source]
    pre-parse source

    ;trace on
    ret: try [parse source rebol/rule]
    ;trace off
;    debug ["block:" mold rebol/val]
    if error? ret [
        fail ret
    ]

    if ret [
        return rebol/val
    ]
]
