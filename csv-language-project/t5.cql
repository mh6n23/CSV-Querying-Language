PROJECT [0,1,2,3] (
    REPLACE [1 -> 5] WHERE [1 == ""] (
        REPLACE [2 -> 6] WHERE [2 == ""] (
            REPLACE [3 -> 7] WHERE [3 == ""] (
                NaturalJoin ("P.csv") ON [0 == 0] ("Q.csv")
            )
        )
    )
)