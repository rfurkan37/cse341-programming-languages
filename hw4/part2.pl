classify(_, SepalWidth, PetalLength, PetalWidth, Class) :-
    (
        (PetalLength =< 2.45) ->
            Class = "Iris setosa";
        (PetalLength =< 4.75) ->
            (PetalWidth =< 1.65) ->
                Class = "Iris versicolor";
            Class = "Iris virginica"
    ;
        (PetalWidth =< 1.75) ->
            (PetalLength =< 4.95) ->
                Class = "Iris versicolor";
        (PetalWidth =< 1.55) ->
            Class = "Iris virginica";
        (PetalLength =< 5.45) ->
            Class = "Iris versicolor";
        Class = "Iris virginica"
    ;
        (PetalLength =< 4.85) ->
            (SepalWidth =< 3.1) ->
                Class = "Iris virginica";
            Class = "Iris versicolor";
        Class = "Iris virginica"
    ;
        Class = "Not found"
    ).