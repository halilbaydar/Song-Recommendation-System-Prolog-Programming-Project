 Halil Baydar
% 2017400297
% compiling: yes
% complete: yes

% artist(ArtistName, Genres, AlbumIds).
% album(AlbumId, AlbumName, ArtistNames, TrackIds).
% track(TrackId, TrackName, ArtistNames, AlbumName, [Explicit, Danceability, Energy,
%                                                    Key, Loudness, Mode, Speechiness,
%                                                    Acousticness, Instrumentalness, Liveness,
%                                                    Valence, Tempo, DurationMs, TimeSignature]).


getArtistTracks(ArtistName, TrackIds, TrackNames) :-helper(ArtistName,TrackIds,TrackNames). % I needed helper predicates to get TrackIds, TrackNames items seperately
helper(ArtistName,TrackIds,TrackNames):-
    findall( TrackIds, (track(TrackIds,TrackNames,[ArtistName],_,_)) , TrackIds),helper2(ArtistName,TrackNames). %findall gets all results that is equal to ArtistName
helper2(ArtistName,TrackNames):-
    findall( TrackNames, (track(_,TrackNames,[ArtistName],_,_)) , TrackNames). %findall gets all results that is equal to ArtistName

%below predicate returns the size of list given paramether
length_for_me([],0).
length_for_me([],_).
length_for_me([_|T],N):-length_for_me(T,N1), N is N1+1.

%this predicate divides whole list items by given size
bol_of_size([],[],0).
bol_of_size([],[],_).
bol_of_size([Head|Tail],EE,Size):-bol_of_size(Tail,EE1,Size), X is (Head/Size), EE=[X|EE1].

%firstly gets tracklist in album.pl file and processes it and then returns
albumFeatures(AlbumId, AlbumFeatures):-album(AlbumId,_,_,TrackList),length_for_me(TrackList, Size),helper(TrackList,DD),sum_of_list(DD,KK),bol_of_size(KK,AlbumFeatures,Size),!.

%this helper predicate traverse all track in album recursively and filters track features
helper([],_).
helper([H|T],DD):-helper(T,DD1),track(H,_,_,_,SS), filter_features(SS,GG), DD=[GG|DD1].

%returns the sum of list items iven parameter
sum_of_list([],[]).
sum_of_list([H|T],Sum):-sum_of_list(T,Sum1), list1_list2_sums(H,Sum1,Sum),!.

%this predicates sums all nested lists in one list and returns
list1_list2_sums([],[],_).
list1_list2_sums(H,[],Sum):-Sum=H.
list1_list2_sums([Head|Tail],[Head2|Tail2],Sum):-list1_list2_sums(Tail,Tail2,Sum2), X is ((Head+Head2)) , Sum=[X|Sum2].

features([explicit-0, danceability-1, energy-1,
          key-0, loudness-0, mode-1, speechiness-1,
       	  acousticness-1, instrumentalness-1,
          liveness-1, valence-1, tempo-0, duration_ms-0,
          time_signature-0]).


%filters features defined in the description
filter_features(Features, Filtered) :- features(X), filter_features_rec(Features, X, Filtered).
filter_features_rec([], [], []).
filter_features_rec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filter_features_rec(FeatTail, Tail, FilteredTail),
    _-Use = Head,
    (
        (Use is 1, FilteredFeatures = [FeatHead|FilteredTail]);
        (Use is 0, FilteredFeatures = FilteredTail
        )
    ).

%get given ArtistName parameter and processes it and then returns
artistFeatures(ArtistName, ArtistFeatures):- artist(ArtistName,_,AlbumId), flatten(AlbumId, AlbumId1),get_trackId(AlbumId1,TrackIds1) ,flatten(TrackIds1, TrackIds2),
get_track_features_from_trackid(TrackIds2,Trackfeatures),length_for_me(Trackfeatures,Size),sum_of_list(Trackfeatures,KK),filter_features(KK,JJ),bol_of_size(JJ,ArtistFeatures,Size),!.

%this predicate returns tracks features by travesing given track id list
get_track_features_from_trackid([],[]).
get_track_features_from_trackid([H|T],[Head|Tail]):-track(H,_,_,_,X),Head=X,get_track_features_from_trackid(T,Tail).

%this predicate traveses all album list and returns track ids
get_trackId([],[]).
get_trackId([H|T],[Head|Tail]):-album(H,_,_,X), Head =X, get_trackId(T,Tail).

%this predicate returns track distance by processing it like in the description
trackDistance(TrackId1, TrackId2, Score):-track(TrackId1,_,_,_,SS),track(TrackId2,_,_,_,SS2),
filter_features(SS,SS1),filter_features(SS2,SS3),hesapla(SS1,SS3,RESULT), listeyi_topla(RESULT,Temp),Score is sqrt(Temp),!.

%compute distance between two tracks
hesapla([],[],_).
hesapla(Head,[],Temp):-Temp=Head.
hesapla([Head|Tail],[Head2|Tail2],Temp):- hesapla(Tail,Tail2,Temp1), X is ((Head-Head2)**2), Temp=[X|Temp1].

%this predicate returns sum of items in list given parameter
listeyi_topla([],0).
listeyi_topla([H|T],RESULT):-listeyi_topla(T,RESULT1), RESULT = (RESULT1+H).

%this predicate computes album features and processes it
albumDistance(AlbumId1, AlbumId2, Score):-albumFeatures(AlbumId1, Temp1),albumFeatures(AlbumId2, Temp2),hesapla(Temp1,Temp2,R),listeyi_topla(R,Sum),Score is sqrt(Sum),!.

%this predicates computes artist features by using helper functions
artistDistance(ArtistName1, ArtistName2, Score):-artistFeatures(ArtistName1, Temp1),artistFeatures(ArtistName2,Temp2),hesapla(Temp1,Temp2,R),listeyi_topla(R,Sum),Score is sqrt(Sum),!.

%this predicates split list from the given index
take_for_me(List, N, Prefix) :-
    length(List, Len),
    (   Len =< N
    ->  Prefix = List
    ;   length(Prefix, N),
        append(Prefix, _, List)
    ).

%this predicate returns similar first 30 tracks to given track
findMostSimilarTracks(TrackId, SimilarIds, SimilarNames):-findall(
     Difference-IdofTrack-Nameoftrack,
     ((track(IdofTrack,Nameoftrack,_,_,_)),
     (trackDistance(TrackId,IdofTrack,Difference))
       ),
       Result),

sort(Result,SortedResult),[_|Tail]=SortedResult, parse_sorted_result(Tail,SimilarIds_temp,SimilarNames_temp),take_for_me(SimilarIds_temp,30,SimilarIds), take_for_me(SimilarNames_temp,30,SimilarNames),!.

%this predicate parses given list items form -
parse_sorted_result([],[],[]).
parse_sorted_result([],_,_).
parse_sorted_result([H|T],SimilarIds,SimilarNames):-parse_sorted_result(T,SimilarIds1,SimilarNames1), _-H1-H2=H ,SimilarIds=[H1|SimilarIds1] , SimilarNames=[H2|SimilarNames1].

%this predicates returns similar first 30 items to given album
findMostSimilarAlbums(AlbumId, SimilarIds, SimilarNames):-findall(
     Difference-IdofAlbum-NameofAlbum,(
        album(IdofAlbum,NameofAlbum,_,_),
        albumDistance(IdofAlbum,AlbumId,Difference)
         ),Result
     )
     ,sort(Result,SortedResult),[_|Tail]=SortedResult, parse_sorted_result(Tail,SimilarIds_temp,SimilarNames_temp),take_for_me(SimilarIds_temp,30,SimilarIds),take_for_me(SimilarNames_temp,30,SimilarNames),!.

%this predicate returns first 30 artist similar to given artist
findMostSimilarArtists(ArtistName, SimilarArtists):-findall(
    Difference-NameofArtist,
    (
        artist(NameofArtist,_,_),
        artistDistance(ArtistName,NameofArtist,Difference)
        )
        ,Result
    ),
    sort(Result,SortedResult),[_|Tail]=SortedResult,parse_sorted_result_for_artist(Tail,SimilarNames_temp),take_for_me(SimilarNames_temp,30,SimilarArtists).

%this predicate parses given list items form -
parse_sorted_result_for_artist([],[]).
parse_sorted_result_for_artist([H|T],SimilarNames):-parse_sorted_result_for_artist(T,SimilarNames1), _-H1=H ,SimilarNames=[H1|SimilarNames1].

%this predicate filters given track list by checking explicit number
filterExplicitTracks(TrackList, FilteredTracks):-helpppp(TrackList,FilteredTracks),!.
helpppp([],[]).
helpppp([H|T],FilteredTracks):-helpppp(T,FilteredTracks2),track(H,_,_,_,FF),
[K|_]=FF,(
    (K =:= 0 ,FilteredTracks = [H|FilteredTracks2]);
    (K =:= 1, FilteredTracks = FilteredTracks2)
    ).

%returns genre list of given track
getTrackGenre(TrackId, Genres):-track(TrackId,_,[ArtistName],_,_),artist(ArtistName,Genres,_).

%this predicate filters disliked and not liked tracks with genres and writes 30 tracks list with id,name,etc
discoverPlaylist(LikedGenres, DislikedGenres, Features, FileName, Playlist):-findall(
    Feature-TrackId-TrackName-ArtistName-Genres,(
        track(TrackId,TrackName,[ArtistName],_,Features_as_default),
        artist(ArtistName,Genres,_),
        filter_features(Features_as_default,Feature)
        ),
        Result
    ),open(FileName, write, Stream),help_for_sum_of_list(Result,ZZ,Features),sort(ZZ,ZZ1),filtrele(ZZ1,DislikedGenres,LikedGenres,Filtered),
    take_for_me(Filtered,30,Filtered1),parse_for_me2(Filtered1,Id,TName,AName,Distance),append(Id,[],Id1),append(AName,[],AName1),append(Distance,[],Distance1),
    Playlist = Id1, writeln(Stream, Id1),append(TName,[],TName1),writeln(Stream,TName1),writeln(Stream,AName1),writeln(Stream,Distance1),close(Stream),!.

%this predicate filters track according to genres and given liked and disliked genres
filtrele([],_,_,_).
filtrele([H1|T1]/*genres*/,Dislike/*dislike*/,Like/*like*/,Filtered):-filtrele(T1,Dislike,Like,Filtered1),
_-_-_-_-Genres=H1, length_for_me(Dislike,Size),length_for_me(Like,Size2),
    (
    (((Size=\=0,is_it_dislike(Genres,Dislike));\+is_it_like(Genres,Like)),Filtered=Filtered1); % ((is it empty ? , is it dislike ?)  ; is it not liked ) so the track is filtered
        ((Size2=\=0,is_it_like(Genres,Like)), Filtered= [H1|Filtered1]) % is it not emty list ? , is it liked egnre with track ?  so it is added
    ).

%this predicate returns true if given genre list substring of track genre
is_it_dislike(_,[]):-false.
is_it_dislike(Genres,[H|T]):-is_it_dislike1(Genres,H);is_it_dislike(Genres,T).

%traverse track genre list by checking if given dislike genre is subtring of track genre or not
is_it_dislike1([],_):-false.
is_it_dislike1([H|T],Dislike):-
    Genre = H,
    (sub_string(Genre,_,_,_,Dislike));
    (is_it_dislike1(T,Dislike)).

%this predicate returns true if given genre list substring of track genre
is_it_like(_,[]):-false.
is_it_like(Genres,[H|T]):-is_it_like1(Genres,H);is_it_like(Genres,T).

%traverse track genre list by checking if given like genre is subtring of track genre or not
is_it_like1([],_):-false.
is_it_like1([H|T],Like):-Genre = H,
    (sub_string(Genre,_,_,_,Like));
    (is_it_like1(T,Like)).

%this helper predicate calculates distance between given features and track features
help_for_sum_of_list([],[],_).
help_for_sum_of_list([H|T],Result,Sfeatures):- help_for_sum_of_list(T,Result1,Sfeatures),
Features-TrackId-TrackName-ArtistName-Genres=H, hesapla(Features,Sfeatures,Difference), listeyi_topla(Difference,Difference1),
Difference2 is sqrt(Difference1), Result=[Difference2-TrackId-TrackName-ArtistName-Genres|Result1].

% splitting all items(Feature-TrackId-TrackName-ArtistName-Genres) in list to write into file
parse_for_me2([],_,_,_,_).
parse_for_me2([],[],[],[],[]).
parse_for_me2([H|T],Id,Tname,Aname,Distance):-parse_for_me2(T,Id1,Tname1,Aname1,Distance1),Uzaklik-Numara-HTName-HAName-_=H , Id=[Numara|Id1],
Tname=[HTName|Tname1], Aname=[[HAName]|Aname1],Distance=[Uzaklik|Distance1].







