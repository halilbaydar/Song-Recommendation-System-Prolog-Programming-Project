dontknow(eda).
dontknow(ece).
kapi_numarasi(X,Y,List,Z):-dontknow(X),return_not_pair(List,List,Z).

return_not_pair([],L,[]).
return_not_pair([H|T],L,List2):-return_not_pair(T,L,List3), Ilk_sayi is mod(H,10),
    (
        ((is_it_pair(Ilk_sayi,L,Value)),(List2=[H|List3]));
        (List2=List3)
    ).

is_it_pair([],[],Value).
is_it_pair(Ilk_sayi,[H|T],Value):-
    (Ilk_sayi =:= mod(H,10),Value=Value1+1);
    (is_it_pair(Ilk_sayi,T,Value1)).

not_inst(Var):-
    \+(\+(Var=0)),
    \+(\+(Var=1)).