with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure ejercicio3Tasks is

   -- funcion para calcular num random
   package Rand_Int is new Ada.Numerics.Discrete_Random(Integer);
   Gen : Rand_Int.Generator;

   function Rand (Max : Integer) return Integer is
   begin
      return Rand_Int.Random(Gen) mod Max + 1;
   end Rand;

   --constantes
   Num_Vacas  : constant Integer := 100;
   Cap_Ordenie : constant Integer := 15;
   Cap_Mangas : constant Integer := 5;
   Cap_Camion : constant Integer := 50;


   --task sala de ordenie
   task Sala_Ordenie is
      entry Entrar (Id : Integer);
      entry Salir  (Id : Integer);
   end Sala_Ordenie;

   task body Sala_Ordenie is
      Dentro : Integer := 0;
   begin
      loop
         select
            when Dentro < Cap_Ordenie =>
               accept Entrar (Id : Integer) do
                  Dentro := Dentro + 1;
                  Put_Line ("La vaca" & Id'Img & " esta¡ entrando al area de ordenie (" &
                            Integer'Image(Dentro) & " dentro)");
               end Entrar;
         or
            accept Salir (Id : Integer) do
               Dentro := Dentro - 1;
               Put_Line ("La vaca" & Id'Img & " esta¡ saliendo del area de ordenie (" &
                            Integer'Image(Dentro) & " dentro)");
            end Salir;
         or
            terminate;
         end select;
      end loop;
   end Sala_Ordenie;


   --vacunacion
   task Vacunacion is
      entry Entrar_Pasillo (Id : Integer);
      entry Entrar_Manga   (Id : Integer);
      entry Salir_Manga    (Id : Integer);
   end Vacunacion;

   task body Vacunacion is
      En_Mangas     : Integer := 0;
      Pasillo_Libre : Boolean := True;
      Cola          : array (0 .. Cap_Mangas - 1) of Integer := (others => 0);
      In_Ptr, Out_Ptr : Integer := 0;
   begin
      loop
         select
            when En_Mangas < Cap_Mangas and Pasillo_Libre =>
               accept Entrar_Pasillo (Id : Integer) do
                  Pasillo_Libre := False;
                  Put_Line ("La vaca" & Id'Img & " esta entrando al area de vacunacion");
               end Entrar_Pasillo;
               delay 0.2;
               Pasillo_Libre := True;

         or
            accept Entrar_Manga (Id : Integer) do
               Cola(In_Ptr) := Id;
               In_Ptr := (In_Ptr + 1) mod Cap_Mangas;
               En_Mangas := En_Mangas + 1;
               Put_Line ("La vaca" & Id'Img & " ocupa una manga (" &
                            Integer'Image(En_Mangas) & " dentro)");
            end Entrar_Manga;

         or
            when En_Mangas > 0 =>
               accept Salir_Manga (Id : Integer) do
                  if Id = Cola(Out_Ptr) then
                     En_Mangas := En_Mangas - 1;
                     Out_Ptr := (Out_Ptr + 1) mod Cap_Mangas;
                     Put_Line ("La vaca" & Id'Img & " esta saliendo del area de vacunacion (" &
                               Integer'Image(En_Mangas) & " dentro)");
                  else
                     -- Si no es su turno, reintenta luego
                     delay 0.05;
                  end if;
               end Salir_Manga;

         or
            terminate;
         end select;
      end loop;
   end Vacunacion;

   --camiones
   task Camiones is
      entry Subir (Id : Integer);
      entry Llenos (Full : out Boolean);
   end Camiones;

      task body Camiones is
      C1, C2 : Integer := 0;
      Llenos_Flag : Boolean := False;
   begin
      loop
         select
            when C1 < Cap_Camion or C2 < Cap_Camion =>
               accept Subir (Id : Integer) do
                  if C1 < Cap_Camion then
                     C1 := C1 + 1;
                     Put_Line ("La vaca" & Id'Img & " esta entrando al Camion 1 (" &
                               Integer'Image(C1) & "/50)");
                  else
                     C2 := C2 + 1;
                     Put_Line ("La vaca" & Id'Img & " esta entrando al Camion 2 (" &
                               Integer'Image(C2) & "/50)");
                  end if;
               end Subir;

         or
            accept Llenos (Full : out Boolean) do
               Full := (C1 = Cap_Camion and C2 = Cap_Camion);
               Llenos_Flag := Full;
            end Llenos;

         or
            terminate;
         end select;

         if Llenos_Flag then
            -- Espera un poco para permitir que todas las vacas terminen sus rendezvous
            delay 1.0;
            exit;
         end if;
      end loop;

      Put_Line ("Ambos camiones llenos");
   end Camiones;


   --vaca
   task type Vaca is
      entry Start (Id : Integer);
   end Vaca;

   task body Vaca is
      My_Id : Integer := 0;
   begin
      accept Start (Id : Integer) do
         My_Id := Id;
      end Start;

      -- Cada vaca decide aleatoriamente si ordenia o vacuna primero
      if Rand(2) = 1 then
         -- ORDENIE
         Sala_Ordenie.Entrar(My_Id);
         delay Duration(Rand(3));
         Sala_Ordenie.Salir(My_Id);

         -- VACUNACION
         Vacunacion.Entrar_Pasillo(My_Id);
         Vacunacion.Entrar_Manga(My_Id);
         delay Duration(Rand(2));
         Vacunacion.Salir_Manga(My_Id);
      else
         -- VACUNACION
         Vacunacion.Entrar_Pasillo(My_Id);
         Vacunacion.Entrar_Manga(My_Id);
         delay Duration(Rand(2));
         Vacunacion.Salir_Manga(My_Id);

         -- ORDEÃ‘E
         Sala_Ordenie.Entrar(My_Id);
         delay Duration(Rand(3));
         Sala_Ordenie.Salir(My_Id);
      end if;

      -- CAMION
      Camiones.Subir(My_Id);
   end Vaca;

   --programa principal
   V : array (1 .. Num_Vacas) of Vaca;
   Full : Boolean := False;

begin
   Rand_Int.Reset(Gen);

   for I in 1 .. Num_Vacas loop
      V(I).Start(I);
   end loop;

   loop
      Camiones.Llenos(Full);
      exit when Full;
      delay 0.5;
   end loop;

   Put_Line ("FIN: Ambos camiones llenos y vacas en el campo.");
end ejercicio3Tasks;
