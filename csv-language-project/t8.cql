PROJECT [0,1,2,3] (   
    REPLACE [3 -> 7] WHERE [7 != ""] (
        REPLACE [2 -> 6] WHERE [6 != ""] (    
            REPLACE [1 -> 5] WHERE [5 != ""] (
                REPLACE [0 -> 4] WHERE [4 != ""] (
                    NaturalJoin ("P.csv") ON [3 == 3] ("Q.csv"))))))