REBOL []

import %../grammar.r

testcases: 0
pass: 0
xpass: make block! 16
failure: make block! 16
assert-scan: procedure [
    f   [file!]
    src [string! binary!]
][
    ++ testcases
    either error? expected: try [load/all src][
        either error? actual: try [
            scan-source src
        ][
            ++ pass
        ][
            print ["actual:" mold actual]
            print ["expected:" mold expected]
            append/only xpass reduce [f src actual expected]
        ]
    ][
        actual: try [scan-source src]
        either expected = actual [
            ++ pass
        ][
            print ["Failure"]
            print ["actual:" mold actual]
            print ["expected:" mold expected]
            append/only failure reduce [f src actual expected]
        ]
    ]
]

for-each f [
    %integer.r
    %decimal.r
    %percent.r
    %money.r
    %time.r
    %date.r
    %word.r
    %array.r
    %string.r
    %word.r
    %refinement.r
    %path.r
    %construct.r
][
    for-each s load/all f [
        assert-scan f s
    ]
]

print [
    "Number of testcases:" testcases newline
    "PASS:" pass "(" to percent! pass / testcases ")" newline
    "Unexpected PASS:" length xpass "(" to percent! (length xpass) / testcases ")" newline
    "Failure:" length failure "(" to percent! (length failure) / testcases ")"
]
write %xpass.r xpass
write %failure.r failure
