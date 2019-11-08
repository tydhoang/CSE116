/**
 * @author Tyler Hoang
 * prog2.pl
 * CSE116
*/

/* Imported Database */
airport( atl, 'Atlanta         ', degmin(  33,39 ), degmin(  84,25 ) ).
airport( bos, 'Boston-Logan    ', degmin(  42,22 ), degmin(  71, 2 ) ).
airport( chi, 'Chicago         ', degmin(  42, 0 ), degmin(  87,53 ) ).
airport( den, 'Denver-Stapleton', degmin(  39,45 ), degmin( 104,52 ) ).
airport( dfw, 'Dallas-Ft.Worth ', degmin(  32,54 ), degmin(  97, 2 ) ).
airport( lax, 'Los Angeles     ', degmin(  33,57 ), degmin( 118,24 ) ).
airport( mia, 'Miami           ', degmin(  25,49 ), degmin(  80,17 ) ).
airport( nyc, 'New York City   ', degmin(  40,46 ), degmin(  73,59 ) ).
airport( sea, 'Seattle-Tacoma  ', degmin(  47,27 ), degmin( 122,17 ) ).
airport( sfo, 'San Francisco   ', degmin(  37,37 ), degmin( 122,23 ) ).
airport( sjc, 'San Jose        ', degmin(  37,22 ), degmin( 121,56 ) ).

flight( bos, nyc, time( 7,30 ) ).
flight( dfw, den, time( 8, 0 ) ).
flight( atl, lax, time( 8,30 ) ).
flight( chi, den, time( 8,45 ) ).
flight( mia, atl, time( 9, 0 ) ).
flight( sfo, lax, time( 9, 0 ) ).
flight( sea, den, time( 10, 0 ) ).
flight( nyc, chi, time( 11, 0 ) ).
flight( sea, lax, time( 11, 0 ) ).
flight( den, dfw, time( 11,15 ) ).
flight( sjc, lax, time( 11,15 ) ).
flight( atl, lax, time( 11,30 ) ).
flight( atl, mia, time( 11,30 ) ).
flight( chi, nyc, time( 12, 0 ) ).
flight( lax, atl, time( 12, 0 ) ).
flight( lax, sfo, time( 12, 0 ) ).
flight( lax, sjc, time( 12, 15 ) ).
flight( nyc, bos, time( 12,15 ) ).
flight( bos, nyc, time( 12,30 ) ).
flight( den, chi, time( 12,30 ) ).
flight( dfw, den, time( 12,30 ) ).
flight( mia, atl, time( 13, 0 ) ).
flight( sjc, lax, time( 13,15 ) ).
flight( lax, sea, time( 13,30 ) ).
flight( chi, den, time( 14, 0 ) ).
flight( lax, nyc, time( 14, 0 ) ).
flight( sfo, lax, time( 14, 0 ) ).
flight( atl, lax, time( 14,30 ) ).
flight( lax, atl, time( 15, 0 ) ).
flight( nyc, chi, time( 15, 0 ) ).
flight( nyc, lax, time( 15, 0 ) ).
flight( den, dfw, time( 15,15 ) ).
flight( lax, sjc, time( 15,30 ) ).
flight( chi, nyc, time( 18, 0 ) ).
flight( lax, atl, time( 18, 0 ) ).
flight( lax, sfo, time( 18, 0 ) ).
flight( nyc, bos, time( 18, 0 ) ).
flight( sfo, lax, time( 18, 0 ) ).
flight( sjc, lax, time( 18,15 ) ).
flight( atl, mia, time( 18,30 ) ).
flight( den, chi, time( 18,30 ) ).
flight( lax, sjc, time( 19,30 ) ).
flight( lax, sfo, time( 20, 0 ) ).
flight( lax, sea, time( 22,30 ) ).

/* distance predicate that calculates how long it would take for a flight from one airport to another, using haversine formula */
distance( Degrees1, Minutes1, Degrees2, Minutes2, Degrees3, Minutes3, Degrees4, Minutes4, D) :-
	Pi is pi/180,
	Lat1 is Degrees1+(Minutes1/60),
	Latitude is Lat1*Pi,
	Long1 is Degrees2+(Minutes2/60),
	Longitude is Long1*Pi,
	Lat2 is Degrees3+(Minutes3/60),
	Latitude2 is Lat2*Pi,
	Long2 is Degrees4+(Minutes4/60),
	Longitude2 is Long2*Pi,
	X is Latitude2-Latitude,
	Y is Longitude2-Longitude,
	P is sin(X/2)**2,
	Q is cos(Latitude)*cos(Latitude2)*sin(Y/2)**2,
	A is P+Q,
	C is 2*atan2(sqrt(A), sqrt(1-A)),
	D is C*3956.

/* print_trip predicate that prints an action in the correct format */
print_trip( (Action, Code, Name, time( Hour, Minute))) :-
   upcase_atom( Code, Upper_code),
   format( "~6s  ~3s  ~s~26|  ~`0t~d~30|:~`0t~d~33|", 
    	[Action, Upper_code, Name, Hour, Minute]),
   nl.

/* compareTime predicate that checks to see if a flight in question takes place after the previous flight has landed */
compareTime(time( Hour1,Minute1), time( Hour2,Minute2)) :-
	TempMinute is Minute1+30,
	NewHour is Hour1+floor(TempMinute/60),
	NewMinute is TempMinute mod 60,
	( 	Hour2 =:= NewHour 
	-> 	Minute2 >= NewMinute 
	; 	Hour2 > NewHour 
	).

/* test predicate that checks to see if a route exists between two airports */
test( TripA, TripB, PrevHour, PrevMinutes, FinalHour, FinalMinutes, OriginalHour, OriginalMinutes) :- 
	flight( TripA, TripB, time( Hour, Minutes )),	% check to see if a flight exists
	compareTime( time( PrevHour, PrevMinutes ), time( Hour,Minutes )),	% check to see if flight takes place after previous flight has landed

	OriginalHour is Hour,	% the original time of this flight is stored so that it can be added to the list and printed later
	OriginalMinutes is Minutes,

	airport(TripA, _, degmin(Degrees1, Minutes1), degmin(Degrees2, Minutes2) ),
	airport(TripB, _, degmin(Degrees3, Minutes3), degmin(Degrees4, Minutes4) ),
	distance( Degrees1, Minutes1, Degrees2, Minutes2, Degrees3, Minutes3, Degrees4, Minutes4, Distance),
	Time is Distance/500,	% planes fly at 500 mph
	Hour1 is floor(Time),	% perform the time addition
	Min is Time-Hour1,
	Min1 is Min*60,
	NewHour is Hour+Hour1,
	NewMinutes is round( Minutes+Min1),
	FinalHour is NewHour+floor(NewMinutes/60),
	FinalMinutes is NewMinutes mod 60.

/* print predicate that prints the list of flights created by flyHelper */
print([]).
print([H|T]) :- print_trip( H), print( T).

/** 
 * fly predicate has only 2 constraints - the desired departure and the desired destination. flyHelper does everything else 
 * (I think McDowell wants fly to only have 2 constraints) 
 */
fly( A, B) :-
	flyHelper( A, B, 0, 0, _, _, []).

/**
 * flyHelper predicate that sees if there is a direct flight between A and B. If not, check to see if there is a flight between A and C, then C and B.
 * This predicate is recursive and will add the valid flights to the list, creating a route between A and B.
 */
flyHelper( TripA, TripB, Hour, Min, _, _, List) :- 
	TripA \= TripB,
	test(TripA, TripB, Hour, Min, EndHour, EndMin, OriginalHour, OriginalMinutes),	% this is a direct flight
	airport( TripA, Name, _, _),
	airport( TripB, Name1, _, _),
	append(List, [( depart, TripA, Name , time( OriginalHour, OriginalMinutes )), ( arrive, TripB, Name1, time( EndHour,EndMin ))], NewList),
	print(NewList).
flyHelper( TripA, TripB, Hour, Min, EndHour, EndMin, List) :- 
	TripA \= TripB,
	test(TripA, TripC, Hour, Min, EndHour, EndMin, OriginalHour, OriginalMinutes),	% this is not a direct flight
	airport( TripA, Name, _, _),
	airport( TripC, Name1, _, _),
	append(List, [( depart, TripA, Name , time( OriginalHour, OriginalMinutes )), ( arrive, TripC, Name1, time( EndHour,EndMin ))], NewList),
	flyHelper( TripC, TripB, EndHour, EndMin, _, _, NewList).	% call flyHelper on C to B, keeping mind of the this flight's end time


main :- read(A),read(B), fly( A, B).
