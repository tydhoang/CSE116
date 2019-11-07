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

print_trip( Action, Code, Name, time( Hour, Minute)) :-
   upcase_atom( Code, Upper_code),
   format( "~6s  ~3s  ~s~26|  ~`0t~d~30|:~`0t~d~33|", 
    	[Action, Upper_code, Name, Hour, Minute]),
   nl.

compareTime(time( Hour1,Minute1), time( Hour2,Minute2)) :-
	(	Hour2 =:= Hour1
	->	Minute2 >= Minute1
	;	Hour2 > Hour1
	).

test( TripA, TripB, HourQ, MinutesQ, FinalHour, FinalMinutes) :- 
	flight( TripA, TripB, time( Hour, Minutes )),
	compareTime( time( HourQ, MinutesQ ), time( Hour,Minutes )),

	airport(TripA, Name, degmin(Degrees1, Minutes1), degmin(Degrees2, Minutes2) ),
	airport(TripB, Name1, degmin(Degrees3, Minutes3), degmin(Degrees4, Minutes4) ),
	distance( Degrees1, Minutes1, Degrees2, Minutes2, Degrees3, Minutes3, Degrees4, Minutes4, Distance),
	Time is Distance/500,
	Hour1 is floor(Time),
	Min is Time-Hour1,
	Min1 is Min*60,
	NewHour is Hour+Hour1,
	NewMinutes is round( Minutes+Min1),
	FinalHour is NewHour+floor(NewMinutes/60),
	FinalMinutes is NewMinutes mod 60,
	
	print_trip( depart, TripA, Name , time( Hour, Minutes )),
	print_trip( arrive, TripB, Name1, time( FinalHour, FinalMinutes)).

fly( A, B) :-
	flyHelper( A, B, 0, 0, _, _).

flyHelper( TripA, TripB, Hour, Min, _, _) :- test(TripA, TripB, Hour, Min, _, _).
flyHelper( TripA, TripB, Hour, Min, EndHour, EndMin) :- test(TripA, TripC, Hour, Min, EndHour, EndMin),
	flyHelper( TripC, TripB, EndHour, EndMin, _, _ ).

main :- read(A),read(B), fly( A, B).
