-- ====================================================
--
-- This LUA reads chess pgn-files M1.pgn,M2.pgn,M3.pgn
-- and prepares 16Kb bank-files to include into .SNA
-- Read "compression_how.txt"
--
-- Files available for downloading:
--   https://chessforeva.blogspot.com/2016/01/gen-mates.html
--
-- ====================================================


-- Lua (www.lua.org)

-- this loads and executes other .lua file
function dofile (filename)
  local f = assert(loadfile(filename))
  return f()
end
dofile( "c0_chess_subroutine.lua" );	-- chess logic

--
-- read pgn file and prepare data
--
-- 

SHORT=0;   -- Set 1 to generate short pgn-file versions too!
SKIP=0;    -- Set 1 if skip some in large pgn-file
--



-- bank numbers
BANKS = { 0,1,3,4,6,7 };


-- Mx-puzzles
MS = { "M1","M2","M2","M2","M3","M3" };
SKp= { 45, 11, 11, 11, 8, 8 };


N=0;     -- all games count
B=0;     -- bad games count
pgn="";  -- pgn of a game
FEN="";  -- FEN from pgn
BAD=0;   -- flag that this game is bad case
Bt = 0;  -- bytes for all data
Pa = 0;  -- count of pieces average

HS  = ""; -- string of bits for this game
HFF = ""; -- string of bits for this 256 bytes
Ggc = 0;  -- count of games in 256 bytes
GcMx = 0; -- max.count of games in 256 bytes reached
WrBy = 0; -- written bytes


WM = 0; BM = 0; -- counts width of moves records for both sides
ToMove = 0;     -- which side to move first

function T(n)
 return c0_LuaChess.ToString(n);
end

function s(a,b,c)
 return c0_LuaChess.Substr(a,b,c);
end
function i(a,b)
 return c0_LuaChess.IndexOf(a,b);
end
function l(s)
 return string.len(s);
end
function iif(a,b,c)  -- iif in Lua
 local r;
 if(a) then
   r=b;
 else
   r=c;
 end
 return r;
end

function Sq(at)  -- "e2"->12
 local h, v;
 h=c0_LuaChess.byteAt(at,0) - 96;
 v=tonumber( s(at,1,1));
 return ((v-1)*8) + (h-1);   -- 0..63
end

function At(sq)  -- 12->"e2"
 local h, v;
 h=(sq % 8); v=(sq-h)/8;
 return c0_LuaChess.c0_convE2 (v+1, h+1)
end

C13 = string.char(10);

M = {}; Mcnt = 0; Mi = -1;  -- moves list
Z = 0; -- index counter

H = {}; Hcnt = 0;  -- all moves to depth

rookMoves = { '+',1,'=',0 ,'-',1,'=',0 ,'=',0,'+',1 ,'=',0,'-',1 };
bishopMoves = { '+',1,'+',1 ,'+',1,'-',1 ,'-',1,'+',1 ,'-',1,'-',1 };
knightMoves = { '+',1,'+',2 ,'+',2,'+',1 ,'+',2,'-',1 ,'+',1,'-',2,
		'-',1,'-',2 ,'-',2,'-',1 ,'-',2,'+',1 ,'-',1,'+',2 };
kingMoves = { '-',1,'=',0 ,'-',1,'+',1 ,'=',0,'+',1 ,'+',1,'+',1,
		'+',1,'=',0 ,'+',1,'-',1 ,'=',0,'-',1 ,'-',1,'-',1 };

-- sorts chess moves by order in asm
function SortMoves()
  local j,at,piece,d,pc,pn,u;
  u=iif( (c0_LuaChess.c0_sidemoves>0), "w", "b" );
  Z=0;
  j=0;			-- scanning squares
  while(j<64) do
   at= At(j);
   piece = c0_LuaChess.c0_D_what_at(at);
   if(l(piece)>0) then
    pc=s(piece,0,1);
    if(pc==u) then	-- if this color

     pn=s(piece,1,1);

     if(pn=="p") then	-- if pawn
      d = iif( (pc=="w"),1,-1 );
      addmv(j,8*d,"QRBN");
      addmv(j,16*d,"");
      addmv(j,7*d,"QRBN");
      addmv(j,9*d,"QRBN");
     end

     if(i( "BQ", pn )>=0 ) then
      addln(j,bishopMoves,8);
     end
     if(i( "RQ", pn )>=0 ) then
      addln(j,rookMoves,8);
     end

     if( pn=="N" ) then
      addln(j,knightMoves,1);
     end
     if( pn=="K" ) then
      if(j==4 or j==60) then
        addmv(j,2,"");
        addmv(j,-2,"");
      end
      addln(j,kingMoves,1);
     end
    end
   end
   j = j+1;
  end

  -- verify and print if there are unsorted
  j=0;
  while(j<=199) do
    q=0;
    while(q<Mcnt) do
      q=q+1;
      if( M[q].i==j ) then
        if(j==199) then		-- if not assigned
          print(":" .. c0_LuaChess.c0_get_FEN() .. "  " .. T(j).." - " .. M[q].mv);
          BAD=1;
        end
      end
    end
    j=j+1;
  end

  -- save current index of ordering
  if(c0_LuaChess.c0_sidemoves>0) then
    if(Z>WM) then
      WM = Z;
    end
  else
    if(Z>BM) then
      BM = Z;
    end
  end

end

-- pawn moves
function addmv(j,n,p)
  local w=j+n;
  if(w>=0 and w<64) then
    fmv(j,w,p);
  end
end

-- scan all directions
function addln(j,a,n)
  local L,k,r,w,wp,dx,dy;
  L = table.getn(a);
  r=0;
  while(r<L) do
   dx= tonumber( a[r+2] );
   if(a[r+1]=='-') then
     dx=-dx;
   end
   dy= tonumber( a[r+4] )*8;
   if(a[r+3]=='-') then
     dy=-dy;
   end
   r=r+4;
   w=j;
   k=n;
   while(k>0) do
     wp=w;
     w=w+dx+dy;

     -- if this outside board
     if(w<0 or w>63) then
       break
     end
     if( (w % 8) - (wp % 8) ~= dx ) then
       break
     end

     fmv(j,w,"");
     k = k-1;
   end
  end
end

-- if this move is in list of possible moves, then assign order index
function fmv(f,t,pp)
  local d,o,p;

  p = pp;
  if((t>7) and (t<56)) then
    p="";		-- not promoted
  end

  d=0;
  while ( d<Mcnt ) do
   d=d+1;
   o=M[d];
   if(o.f==f and o.t==t) then
     o.i = Z;
     Z = Z+1;
     o.q = p;

     if(l(p)>0) then
       Z = Z+3;		-- Q + RBN
     end
     break;
   end
  end
end

-- prepare chess data (encode)
function ChessData()

  local mlist, k, mv, m, o, g, w, j, sqf, sqt, zsq, f0,t0,p0, f,t,p, q, e;

  if(l(pgn)>0) then

    BAD=0;
    N = N + 1;
    WM = 0; BM = 0;

    c0_LuaChess.c0_start_FEN=FEN;
    c0_LuaChess.c0_set_start_position("");

    mlist = c0_LuaChess.c0_get_moves_from_PGN(pgn);
    c0_LuaChess.c0_set_FEN(FEN);

    c0_LuaChess.c0_PG_viewer = false; -- prep.
    ToMove = c0_LuaChess.c0_sidemoves;
    
    H = {}; Hcnt = 0;

    HS = encodePos() .. iif( ToMove>0, "0", "1" );

    k=0;
    while( k<l(mlist)) do
      m = s(mlist,k,4);
      p0="Q";
      k=k+4;
      if( k<l(mlist) and s(mlist,k,1)=="[" ) then
        m=m..s(mlist,k,3);
        p0 = s(mlist,k+1,1);
        k=k+3;
      end
      f0 = s(m,0,2);
      t0 = s(m,2,2);

      sqf = Sq(f0);
      sqt = Sq(t0);

      M = {}; Mcnt = 0; Mi = -1;

      mv = c0_LuaChess.c0_get_next_moves();

      w = 0;
      j = 0;
      while( w<l(mv)) do
        g = s(mv,w,4);
        f = Sq( s(g,0,2) );
        t = Sq( s(g,2,2) );
        p = "Q";
        if(s(mv,w+4,1)=="[" ) then
          p=s(mv,w+5,1);
          g = g .. p;
          w=w+3;
        end
        w=w+5;
        j=j+1;

        o = {}; o.f = f; o.t = t; o.p = p; o.mv = g; o.i = 199; o.Th = 0;

        Mcnt = Mcnt + 1;
        M[ Mcnt ] = o;

        if(sqf==f and sqt==t) then
          Mi = Mcnt;
          o.Th = 1;
        end

      end

      SortMoves();

      e=i("QRBN",p0);
      q = {};
      q.i = e + M[ Mi ].i;

      q.Sd = c0_LuaChess.c0_sidemoves;

      Hcnt = Hcnt + 1;
      H[ Hcnt ] = q;

      c0_LuaChess.c0_become_from_engine = p0;
      c0_LuaChess.c0_move_to(f0,t0);
      c0_LuaChess.c0_sidemoves = -c0_LuaChess.c0_sidemoves;

    end

    if(WM>64 or BM>64) then	-- ignore such cases
      print("one bad ignored: "..iif((ToMove>0), T(WM), T(BM)) .. " moves possible");
      BAD=1;
    else
      HS = HS .. movetableId();
    end

    k=0;
    while(k<Hcnt) do
      k = k+1;
      q = H[k];
      n = iif( (q.Sd>0), WM, BM );
      HS = HS .. s( d2b( q.i ), 8-n,n);	-- move index to bits
    end

    if(BAD==1) then
      B = B+1;
    else
      k=l(HS);
      HS = d2b(k) .. HS;	-- add b0 = count of bits
      n=(k%8);
      if(n>0) then		-- add tail bits
        n=8-n;
        k=k+n;
        while(n>0) do
          HS = HS .. "0";
          n=n-1;
        end
      end
      k=k/8;
      Bt = Bt + 1 + k;


      k=l(HS);
      n=l(HFF);

      -- each 256 bytes
      if((n+k)>2047) then
        while(n<2047) do
          HFF=HFF.."0";
          n = n+1;
        end
        HFF= d2b(Ggc) .. HFF;	-- now 256 bytes
        if(Ggc>GcMx) then
          GcMx = Ggc;
        end

        ou_file:write( s2data(HFF) );
        WrBy = WrBy + 256;
        HFF = "";
        Ggc = 0;
      end
      HFF=HFF..HS;
      Ggc = Ggc + 1;

    end

  end

end

PCi = "{wp}{wN}{wB}{wR}{wQ}{wK}{bp}{bN}{bB}{bR}{bQ}{bK}";


-- converts number 0-255 to binary
function d2b(n)
  local b,r;
  b=128;
  r=n;
  t="";
  while(b>=1) do
    if(r>=b) then
      r=r-b;
      t=t.."1"
    else
      t=t.."0"
    end
    b=b/2;
  end
  return t;
end

-- writes bytes to output data file
function s2data(z)
  local b,k,L;
  L = l(z);
  k = 0;
  while(k<L) do
    if(k % 8 == 0) then
      b = 128;
      n = 0;
    end
    if( s(z,k,1)=="1" ) then
      n = n + b;
    end
    if(b==1) then
      ou_file:write(string.char(n));
    else
      b = b/2;
    end
    k = k+1;
  end
end


-- encode position and return string
function encodePos()

  local z,j,piece,at,k;
  z="";
  j=56;			-- scanning squares A8-H8,A7-H7,...A1-H1
  while(j>=0) do
   at= At(j);
   k=0;
   piece = c0_LuaChess.c0_D_what_at(at);
   if(l(piece)>0) then
    k=1+(i(PCi,"{"..piece.."}")/4);
    z=z.."0".. s( d2b(k), 4,4) .. s( d2b(j%8), 5,3);

    Pa = Pa + 1;
   end
   j = j+1;
   if(j%8==0) then
     z=z.."1";  -- next rank line
     j = j-16;
   end
  end
  return z;
end

TB = { 16,8, 32,8, 32,16, 32,32, 64,8, 64,16, 64,32, 64,64 };

-- encode position and return string
function movetableId()
  local wm,bm,z, n,k,x1,x2;
  z="";
  n=0;
  while(n<8 and l(z)==0) do
   k=(n*2);
   if(ToMove<0) then  -- swap move records
     wm=BM; bm=WM;
   else
     wm=WM; bm=BM;
   end
   x1 = TB[k+1];
   x2 = TB[k+2];
   if( wm<=x1 and bm<=x2 ) then
     z=s( d2b(n), 5,3);

     wm = 7-i( d2b(x1),"1");
     bm = 7-i( d2b(x2),"1");
     if(ToMove<0) then
       BM=wm; WM=bm;
     else
       WM=wm; BM=bm;
     end
   end
   n=n+1;
  end
  return z;
end


-- write game moves too for short pgn version
function powrgm()
  local b = 0;
  while(SHORT==1 and in_file~=nil) do
    buf = in_file:read();
    if(not buf)then break end;
    if(l(buf)>0) then
      if(i(buf,"FEN")>0) then
        b = b + 1;
        if(b>9) then
          break
        end
      end
    end
    po_file:write(buf..C13);
  end
end


-- read pgn-file

WrMAXLEN=(16*1024)-256;		-- 0xC000 - 0xFE00

BA = 0;   -- for all 5 banks
FN = "";

while(BA<table.getn(BANKS)) do

  BA = BA + 1;
  Mname = MS[BA];
  Bname = "BANK"..T(BANKS[BA])..".bin";


  if(FN~=Mname) then
    if( l(FN)>0 ) then
      powrgm();
      in_file:close();
      if(SHORT==1) then
        po_file:close();
      end
    end
    FN = Mname;
    in_file = io.open(FN..".pgn", "r");
    if(in_file==nil) then
      print(C13.."UnZip Mx.zip for chess pgn-files!"..C13);
      break
    else
      if(BA==1) then

         print(C13.."Packing chess pgn-data into binary BANK-files" .. C13 ..
           " to include into .SNA compilation, look ChessZX.asm"..C13);
      end
    end
    if(SHORT==1) then
      po_file = assert(io.open(FN.."short.pgn", "w"));
    end
  end

  ou_file = assert(io.open(Bname, "wb"));

  HS  = "";
  HFF = "";
  Ggc = 0;
  WrBy = 0;
  GcMx = 0;

  N=0; B=0;
  Bt = 0; Pa = 0;

  print("File:" .. Bname .. ", puzzles " .. Mname);

  while (WrBy~=WrMAXLEN) do
    buf = in_file:read();

     -- if should skip some
    if((SKIP==1) and (i(buf,"FEN")>0)) then
      Skip=SKp[BA];
      while(Skip>0) do
        buf = in_file:read();
        if(not buf) then break end;
        if(i(buf,"FEN")>0) then
          Skip = Skip-1;
        end
      end
    end

    if(SHORT==1) then
      po_file:write(buf..C13);
    end
    if(not buf)then break end;

    if(l(buf)>0) then
      a = i(buf,"FEN");
      if(a>0) then
        ChessData();
	FEN=s(buf,6,l(buf)-9);
	pgn = "";
	if( N>0 and (N % 300 == 0) ) then
	  print("N="..T(N) .. ",B="..T(B) .. ",Bytes="..T(WrBy));
	end
      end
      pgn = pgn .. " " .. buf;
    end
  end

  if(WrBy~=WrMAXLEN) then
    ChessData();
  end

  ou_file:close();

  print("Wrote " .. "N="..T(N) .. ", B="..T(B) ..
    ", Bytes=" .. T(WrBy) .. ", Bytes/pos=" ..
     T(WrBy/N) .. ", pieces/pos=" .. T(Pa/N)..
     " max.gc.=" .. T(GcMx).. C13);

end

powrgm();

if(in_file~=nil) then
 in_file:close();
 if(SHORT==1) then
   po_file:close();
 end
end
