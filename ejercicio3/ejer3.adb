with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;

procedure ejer3 is

   --funcion para calcular num random
   package Rand_Int is new Ada.Numerics.Discrete_Random (Integer);
   Gen : Rand_Int.Generator;

   function Rand (Max : Integer) return Integer is
   begin
      return Rand_Int.Random (Gen) mod Max + 1;
   end Rand;



   -- defino constantes
   Num_Vacas     : constant Integer := 100;
   Cap_Ordeñe    : constant Integer := 15;
   Cap_Mangas    : constant Integer := 5;
   Cap_Camion    : constant Integer := 50;


   -- sala de ordeñe
   protected Sala_Ordeñe is
      entry Entrar (Id : Integer);
      procedure Salir (Id : Integer);
   private
      Dentro : Integer := 0;
   end Sala_Ordeñe;

   protected body Sala_Ordeñe is
      entry Entrar (Id : Integer) when Dentro < Cap_Ordeñe is
      begin
         Dentro := Dentro + 1;
         Put_Line ("La vaca" & Id'Img & " está entrando al área de ordeñe");
      end Entrar;

      procedure Salir (Id : Integer) is
      begin
         Dentro := Dentro - 1;
         Put_Line ("La vaca" & Id'Img & " está saliendo al área de ordeñe");
      end Salir;
   end Sala_Ordeñe;


   -- area de vacunación
   protected Vacunacion is
      entry Entrar_Pasillo (Id : Integer);
      entry Entrar_Manga   (Id : Integer);
      procedure Salir_Manga (Id : Integer);
   private
      En_Mangas : Integer := 0;
      Pasillo_Libre : Boolean := True;
   end Vacunacion;

   protected body Vacunacion is

      entry Entrar_Pasillo (Id : Integer)
        when En_Mangas < Cap_Mangas and Pasillo_Libre
      is
      begin
         Pasillo_Libre := False;
         Put_Line ("La vaca" & Id'Img & " está entrando al área de vacunación");
      end Entrar_Pasillo;

      entry Entrar_Manga (Id : Integer) when True is
      begin
         En_Mangas := En_Mangas + 1;
         Pasillo_Libre := True;
      end Entrar_Manga;

      procedure Salir_Manga (Id : Integer) is
      begin
         En_Mangas := En_Mangas - 1;
         Put_Line ("La vaca" & Id'Img & " está saliendo al área de vacunación");
      end Salir_Manga;

   end Vacunacion;



   -- camiones
   protected Camiones is
      entry Subir (Id : Integer);
      function Llenos return Boolean;
   private
      C1 : Integer := 0;
      C2 : Integer := 0;
   end Camiones;

   protected body Camiones is

      entry Subir (Id : Integer)
        when C1 < Cap_Camion or else C2 < Cap_Camion
      is
      begin
         if C1 < Cap_Camion then
            C1 := C1 + 1;
            Put_Line ("La vaca" & Id'Img & " está entrando al Camión 1");
         else
            C2 := C2 + 1;
            Put_Line ("La vaca" & Id'Img & " está entrando al Camión 2");
         end if;
      end Subir;

      function Llenos return Boolean is
      begin
         return C1 = Cap_Camion and C2 = Cap_Camion;
      end Llenos;

   end Camiones;


   -- tarea vaca
   task type Vaca is
      entry Start (Id : Integer);
   end Vaca;

   task body Vaca is
      My_Id : Integer := 0;
   begin
      accept Start (Id : Integer) do
         My_Id := Id;
      end Start;

      -- ORDEÑE
      Sala_Ordeñe.Entrar (My_Id);
      delay Duration (Rand(3));
      Sala_Ordeñe.Salir (My_Id);

      -- VACUNACIÓN
      Vacunacion.Entrar_Pasillo (My_Id);
      Vacunacion.Entrar_Manga (My_Id);
      delay Duration (Rand(2));
      Vacunacion.Salir_Manga (My_Id);

      -- CAMIONES
      Camiones.Subir (My_Id);
   end Vaca;


   -- proceso de las 100 vacas
   V : array (1 .. Num_Vacas) of Vaca;

begin
   Rand_Int.Reset(Gen);

   -- Iniciar cada vaca con su ID
   for I in 1 .. Num_Vacas loop
      V(I).Start(I);
   end loop;

   -- Esperar final
   while not Camiones.Llenos loop
      delay 0.5;
   end loop;

   Put_Line ("FIN: Ambos camiones llenos");
end ejer3;
