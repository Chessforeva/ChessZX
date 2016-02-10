--
-- Reads bmp->raw file  24-bits=(B,G,R)
--
-- Prepares data file output .asm as assembler text

filename = "fig24bit.bmp"

--
--
print("Writing Pieces_img.asm");
ou_file = assert(io.open("Pieces_img.asm", "wb"));

PF = { "King", "Queen", "Rook", "Bishop", "Knight", "Pawn", "cursor" };
CR = string.char(13) .. string.char(10);

A = 1
while A<= table.getn(PF) do


 fname = PF[A];

 PS = {};
 PZ = {};

 in_file = assert(io.open(fname..".bmp", "rb"));

 in_file:read(54);	-- skip header
 s = "";
 n = 0;
 c = 0;
 while (1==1) do

    local buf = in_file:read(3);
    if not buf then break end;

    R = string.byte(buf,1);
    G = string.byte(buf,2);
    B = string.byte(buf,3);
    Black = (R<2 and G<2 and B<2);
    White = (R>200 and G>200 and B>200);
    Blue = ((not White) and (not Black));

    if(Black) then
      s = s .. "0";
    else
      s = s .. "1";
    end

    n = n+1;
    if(n==8) then
      c = c + 1
      PS[c] = s;
      n = 0
      s = ""
    end

 end

 ou_file:write(fname .. "_img:" .. CR);

 m = 0;
 w = 0;
 r = 1;
 while(r<=3) do

  n=1;
  while(n<=8) do

   s = "";
   k=1;
   while(k<=4) do
     w = w + 1;
     k = k+1;
     s = s..PS[w].." ";
   end

   m = m + 1;
   PZ[m] = s;

   n = n+1;
  end
  r = r+1;

  m = m + 1;
  PZ[m] = "";
 end


 while (m>0) do
   ou_file:write(" ; " .. PZ[m] .. CR);
   m = m-1;
 end

 ou_file:write(CR..CR);

 t = 0;
 w = w-4;
 while (w>=0) do

   k=1;
   while(k<=4) do

     j = (w+k);
     k = k+1;   
     n=1;
     t = t + 1;
     ou_file:write("; " .. t .. ". of 12" .. CR);

     while(n<=8) do
       n = n+1;
       ou_file:write("  .byte %" .. PS[j] .. CR);
       j = j-4;
     end

     ou_file:write(CR);

   end
   w = w-(4*8);
 end

 ou_file:write(CR..CR);

 in_file:close()


 A=A+1;

end

ou_file:close()

print("Ok");

