
/*This part is inherited from: swish.1.0.1*/
connected(bond_street,oxford_circus,central).
connected(oxford_circus,tottenham_court_road,central).
connected(bond_street,green_park,jubilee).
connected(green_park,charing_cross,jubilee).
connected(green_park,piccadilly_circus,piccadilly).
connected(piccadilly_circus,leicester_square,piccadilly).
connected(green_park,oxford_circus,victoria).
connected(oxford_circus,piccadilly_circus,bakerloo).
connected(piccadilly_circus,charing_cross,bakerloo).
connected(tottenham_court_road,leicester_square,northern).
connected(leicester_square,charing_cross,northern).
/*This is the end of inheritance.*/

reachable(X,Y):-connected(X,Y,_).
reachable(X,Y):-connected(X,Z,_),reachable(Z,Y).


% reachable(Z,Y):-connected(Z,W,L),reachable(W,Y)


