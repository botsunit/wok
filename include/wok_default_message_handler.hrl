-define(WOK_VERSION, 1).
-define(DEFAULT_HEADERS_COMPRESS, true).
-define(DEFAULT_HEADERS, #{compress => ?DEFAULT_HEADERS_COMPRESS}).

-define(TYPE_STRING, 1).
-define(TYPE_INT, 10).
-define(TYPE_UINT, 11).
-define(TYPE_FLOAT, 20).
-define(TYPE_LIST, 30).
-define(TYPE_MAP, 40).
-define(TYPE_BOOLEAN_TRUE, 50).
-define(TYPE_BOOLEAN_FALSE, 51).
-define(TYPE_ATOM, 60).

-define(INT_MIN(X), -1 * math:pow(2, X - 1)).
-define(INT_MAX(X), math:pow(2, X - 1) - 1).
-define(INT_UMIN(_), 0).
-define(INT_UMAX(X), math:pow(2, X) - 1).

-define(INT_SIZES, [8, 16, 32, 64, 128]).

