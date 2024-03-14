% Facts for delivery guys
delivery_guy(p1, 30, 16, o1, inst_x).
delivery_guy(p2, 30, 20, none, library).
delivery_guy(p3, 30, 24, none, admin).

% Facts for places
place(admin).
place(engineering).
place(hall_a).
place(inst_x).
place(inst_y).
place(library).
place(cafe).
place(ss_bld).

% Facts for objects
object(o1, 5, admin, ss_bld, low, p1).
object(o2, 10, admin, inst_y, low, none).
object(o3, 25, library, hall_a, high, none).
object(o4, 20, hall_a, inst_y, high, none).
object(o5, 15, library, inst_y, high, none).

% Facts for the map
edge(admin, engineering, 3).
edge(admin, library, 1).
edge(admin, cafe, 4).
edge(engineering, library, 5).
edge(engineering, hall_a, 2).
edge(hall_a, inst_y, 3).
edge(library, inst_y, 3).
edge(library, cafe, 5).
edge(library, ss_bld, 2).
edge(cafe, ss_bld, 2).
edge(ss_bld, inst_x, 8).

% There is no time between same place so rule for that
edge(X, Y, 0) :- X == Y.

% There is no difference between from cafeteria to social studies building and from social studies building to cafe so rule for that
connected(X,Y, Time) :- edge(X, Y,Time) ; edge(Y,X,Time).

% Rule for shortest path time
shortest_path_time(Start, End, ShortestTime, Path) :-
    findall(
        (Time, P),
        path_time(Start, End, Time, [], P),
        TimesPaths
    ),
    sort(TimesPaths, SortedTimesPaths),
    SortedTimesPaths = [(ShortestTime, Path)|_].


% Direct calculation if they are directly connected
path_time(Start, End, Time, Visited, [End|Visited]) :-
    connected(Start, End, Time).


% Indirect connection
path_time(Start, End, TotalTime, Visited, Path) :-
    connected(Start, Intermediate, Time),
    Intermediate \= End,
    \+ member(Intermediate, Visited),
    path_time(Intermediate, End, TimeRest, [Intermediate|Visited], Path),
    TotalTime is Time + TimeRest.

% Every body can carrry above the weight of object and not carrying object at the moment are available
available_delivery_person(PersonID, ObjectID, TotalTime) :-
    delivery_guy(PersonID, Capacity, _, none, PersonnelLocation),
    object(ObjectID, Weight, CurrentLocation, Destination, _, _),
    Capacity >= Weight,
    shortest_path_time(PersonnelLocation, CurrentLocation, Time1, _),
    shortest_path_time(CurrentLocation, Destination, Time2, _),
    TotalTime is Time1 + Time2.

delivery_in_transit(ObjectID, PersonID, TotalTime) :-
    object(ObjectID, _, _, _, _, PersonID),
    delivery_guy(PersonID, _, _, ObjectID, PersonnelLocation),
    shortest_path_time(PersonnelLocation, _, Time, _),
    TotalTime is Time.

delivery_status(ObjectID, PersonID, TotalTime) :-
    object(ObjectID, _, _, _, _, PersonID),
    (PersonID = none ->
        (setof((Person, Time), available_delivery_person(Person, ObjectID, Time), AvailablePersons),
        (AvailablePersons = [] ->
            write('No available delivery person for the object')
        ; print_available_persons(AvailablePersons)
        ))
    ; (delivery_in_transit(ObjectID, PersonID, TotalTime) ->
        write('Object is already in delivery by Person ID: '), write(PersonID), nl, write('Remaining time: '), write(TotalTime)
    ; write('Error: Object is assigned to a person but not in transit')
    )).

print_available_persons([]).
print_available_persons([(Person, Time)|Rest]) :-
    write('Available delivery person: '), write(Person), nl,
    write('Time to take: '), write(Time), nl,
    print_available_persons(Rest).









