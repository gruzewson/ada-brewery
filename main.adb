--Olaf Jedlinski 193415, Marcel Gruzewski 193589
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
use Ada.Text_IO.Unbounded_IO;




procedure Simulation is

   package My_Exception_Package is
   My_Exception : exception;
end My_Exception_Package;






    --------------------------------- Declarations-----------------------------------------------------

    MAX_UNSUCCESSFUL_ATTEMPTS : constant Integer := 20;

    WODA : constant Integer := 1;
    CHMIEL: constant Integer := 3;

    NUMBER_OF_PRODUCTS  : constant Integer := 6;
    NUMBER_OF_ASSEMBLIES : constant Integer := 4;
    NUMBER_OF_CONSUMERS  : constant Integer := 2;


    subtype Product_Type is Integer range 1 .. NUMBER_OF_PRODUCTS;
    subtype Assembly_Type is Integer range 1 .. NUMBER_OF_ASSEMBLIES;
    subtype Consumer_Type is Integer range 1 .. NUMBER_OF_CONSUMERS;

    Product_Name  : constant array (Product_Type) of Unbounded_String   :=
    (
        To_Unbounded_String("Woda"),
        To_Unbounded_String("Slod"),
        To_Unbounded_String("Chmiel"),
        To_Unbounded_String("Drozdze"),
        To_Unbounded_String("Sok"),
        To_Unbounded_String("Rtec")
    );

    Assembly_Name  : constant array (Assembly_Type) of Unbounded_String   :=
    (
        To_Unbounded_String("Zubr"),
        To_Unbounded_String("Garage"),
        To_Unbounded_String("Perla Export"),
        To_Unbounded_String("SFD Specjal")
    );

    package Random_Assembly is new Ada.Numerics.Discrete_Random
     (Assembly_Type);


    type My_Str is new String (1 .. 256);

    -- Producer produces determined product
    task type Producer is
        -- Give the Producer an identity, i.e. the product type
        entry Start (Product : in Product_Type; Production_Time : in Integer);
    end Producer;

    -- Consumer gets an arbitrary assembly of several products from the buffer
    task type Consumer is
        -- Give the Consumer an identity
        entry Start
           (Consumer_Number : in Consumer_Type; Consumption_Time : in Integer);
    end Consumer;

    -- In the Buffer, products are assemblied into an assembly
    task type Buffer is
        -- Accept a product to the storage provided there is a room for it
        entry Take
           (Product : in Product_Type; Number : in Integer);
        -- Deliver an assembly provided there are enough products for it
        entry Deliver (Assembly : in Assembly_Type; Number : out Integer);
    end Buffer;

        producer_arr : array (1 .. Number_Of_Products) of Producer;
        consumer_arr  : array (1 .. Number_Of_Consumers) of Consumer;
        Buffor       : Buffer; -- i had to do it beacause ada is not case sensitive --TO NASZE? XD

    ---------------------------Declarations-------------------------------------



    task body Producer is

        subtype Production_Time_Range is
           Integer range 0 .. 1;
        package Random_Production is new Ada.Numerics.Discrete_Random(Production_Time_Range);

        Producer_time_Generator : Random_Production.Generator;
        Product_Type_Index : Integer;
        Product_Number      : Integer;
        Time_of_Production          : Integer;

    begin
        accept Start (Product : in Product_Type; Production_Time : in Integer)
        do
            Random_Production.Reset(Producer_time_Generator);
            Product_Number     := 1;
            Product_Type_Index := Product;
            Time_of_Production := Production_Time;
        end Start;

        Put_Line ("Poczatek produkcji " & Product_Name (Product_Type_Index));
        New_Line;

        loop
            delay Duration(Random_Production.Random(Producer_time_Generator));
            Put_Line("Wyprodukowano" & Integer'Image (Product_Number) & " skladnik o nazwie : '" & Product_Name (Product_Type_Index) & "'");
            New_Line;
            Buffor.Take (Product_Type_Index, Product_Number);
            Product_Number := Product_Number + 1;
        end loop;
    end Producer;

    task body Consumer is
        subtype Consumption_Time_Range is Integer range 3 .. 5;
        package Random_Consumption is new Ada.Numerics.Discrete_Random(Consumption_Time_Range);

        Consumption_time_Generator : Random_Consumption.Generator;
        Assembly_index_Generator   : Random_Assembly.Generator;
        Consumer_Index       : Consumer_Type;
        Assembly_Number      : Integer;
        Time_of_Consumption  : Integer;
        Assembly_Type_Index   : Integer;

        Consumer_Name   : constant array (1 .. Number_Of_Consumers) of Unbounded_String :=
        (To_Unbounded_String("Zabka"),
        To_Unbounded_String( "Biedronka")
        );

    begin
        accept Start
           (Consumer_Number : in Consumer_Type; Consumption_Time : in Integer)
        do
            Random_Consumption.Reset(Consumption_time_Generator);
            Consumer_Index := Consumer_Number;
            Time_of_Consumption := Consumption_Time;
        end Start;

      Put_Line ("Rozpoczeto obsluge klienta: " & Consumer_Name (Consumer_Index));
      New_Line;

        loop
            Put_Line("Konsument czeka w kolejce aby zlozyc zamowienie");
            New_Line;
            delay Duration(Random_Consumption.Random (Consumption_time_Generator));
            Put_Line("Konsument zlozyl zamowienie i czeka na odbior");
            New_Line;
            Assembly_Type_Index := Random_Assembly.Random (Assembly_index_Generator);
            select
                Buffor.Deliver(Assembly_Type_Index, Assembly_Number);
                if Assembly_Number /= 0 then
                 Put_Line(Consumer_Name (Consumer_Index) & ": przyjal" & Integer'Image (Assembly_Number) & " partie produktu: " &Assembly_Name (Assembly_Type_Index));
                end if;

            else
                Put_Line("Obecnie browar przyjmuje dostawe wiec nie jest w stanie obsluzyc klienta");
                New_Line;
            end select;
        end loop;
    end Consumer;

    task body Buffer is
        Storage_Capacity : constant Integer := 10;
        type Storage_type is array (Product_Type) of Integer;

        Storage : Storage_type := (0, 0, 0, 0, 0, 0); -- tutaj beda przechowywane skladniki i ich ilosc

        Assembly_Content_Matrix : array (Assembly_Type, Product_Type) of Integer :=
           ((1, 2, 0, 2, 0, 0), --zubr
            (5, 1, 1, 0, 1, 0), --garage
            (1, 2, 1, 2, 0, 0), --perla export
            (4, 0, 1, 0, 0, 1)  --kfd specjal
            ); -- przepisy na dane produkty

        Max_Assembly_Content : array (Product_Type) of Integer;             --  maksymalna liczba skladnikow danego typu
        Assembly_Number : array (Assembly_Type) of Integer := (1, 1, 1, 1); --  zlicza wytworzone produkty

        In_Storage : Integer:= 0;    -- jak duzo produktow w storage
        Cant_Produce_Flag :Integer := 0;

        procedure Setup_Variables is
        begin
            for W in Product_Type loop
                Max_Assembly_Content (W) := 0;
                for Z in Assembly_Type loop
                    if Assembly_Content_Matrix (Z, W) > Max_Assembly_Content (W) then
                        Max_Assembly_Content (W) := Assembly_Content_Matrix (Z, W);
                    end if;
                end loop;
            end loop;
        end Setup_Variables;

        function Can_Accept (Product : Product_Type) return Boolean is
            Free_Room_In_Storage    : Integer;
            Lacking_Products : array (Product_Type) of Integer;
            Lacking_room : Integer;
            Can_Add      : Boolean;

        begin
            -- sprawdza czy jest miejsce
            if In_Storage >= Storage_Capacity then
                return False;
            end if;
            Free_Room_In_Storage := Storage_Capacity - In_Storage;
            Can_Add  := True;

            -- sprawdza czy jest miejsce na kazdy skladnik
            for W in Product_Type loop
                if Storage (W) < Max_Assembly_Content (W) then -- jest za duzo skladnikow tego typu
                   Can_Add := False;
                end if;
            end loop;

            if Can_Add then
                return True; --  storage has products for arbitrary assembly
            end if;

            -- jest miejsce na ten produkt
            if Integer'Max(0, Max_Assembly_Content (Product) - Storage (Product)) > 0
            then return True;
            end if;

            --  mozna dodac produkt
            Lacking_room := 1;

            --ta petla wypelnia ile brakuje konkretnych produktow i ile jest miejsca jeszcze
            for W in Product_Type loop
                Lacking_Products (W)  := Integer'Max (0, Max_Assembly_Content (W) - Storage (W));
                Lacking_room := Lacking_room + Lacking_Products(W); -- sprawdza ile jest zajetych miejsc
            end loop;

            if Free_Room_In_Storage >= Lacking_room then -- jezeli jest wiecej wolnych miejsc niz brakujacych elementow
                return True;
            else
                return False;
            end if;
        end Can_Accept;


        function Can_Deliver (Assembly : Assembly_Type) return Boolean is
        begin
            for W in Product_Type loop
                if Storage (W) < Assembly_Content_Matrix (Assembly, W) -- za malo skladnikow
                then return False;
                end if;
            end loop;
            return True;
        end Can_Deliver;

        procedure Clean_buffer is
        begin
        Storage := (0,0,0,0,0,0);
        In_Storage := 0;
        Cant_Produce_Flag := 0;
        end Clean_buffer;


        procedure Storage_Contents is
        begin
            delay Duration(0.5);
            for W in Product_Type loop
                Put_Line("Magazyn zawiera: " & Integer'Image(Storage(W)) & " "
		       & Product_Name(W));
            end loop;
            New_Line;
      end Storage_Contents;

      -- modyfikacja kodu - exception
      procedure is_Buffer_overloaded is
               begin
         if In_Storage = Storage_Capacity then
            Put_Line ("Exception is raised, Buffer is overloaded");
                     raise My_Exception_Package.My_Exception;
                  end if;
               end is_Buffer_overloaded;

    begin
      Put_Line ("W browarze rozpoczyna sie dzien pracy:");
      New_Line;
        Setup_Variables;

        loop
            accept Take (Product : in Product_Type; Number : in Integer) do
                if Can_Accept (Product) then
                    Put_Line ("Zaakceptowano produkt: " & Product_Name (Product) & " numer: " & Integer'Image (Number));
                    New_Line;
                    Storage (Product) := Storage (Product) + 1;
                    In_Storage        := In_Storage + 1;

                    -- ZEBY MAGAZYN MIAL ODPOWIEDNIE PROPORCJE SKLADNIKOW
                    for W in Product_Type loop
                       if Storage(W) >= 3  and then W /= WODA  and then W /= CHMIEL then --woda(1) i chmiel(3) nie moga byc oddane, potrzeba ich do wszystkiego
                          Storage(W) := Storage(W) / 3;
                          New_Line;
                          Put_Line("Zostalo wyprodukowane za duzo " & Product_Name(W) & ". Czesc zostala oddana pracownikom w ramach premii za 16-godzinne zmiany.");
                          New_Line;
                          In_Storage := In_Storage - Storage(W);
                       end if;
                    end loop;
                    Put_Line("W magazynie jest: " & Integer'Image(In_Storage));

                    else
                    Put_Line
                    ("Browar jest obladowany zamowieniami, pracownicy sie buntuja i nie przyjmuja kolejnych towarów. Dostawa: " & Product_Name (Product) & " numer: " & Integer'Image (Number)
                     & "zostaje przekierowana do innego browaru");
                    New_Line;
                end if;
            end Take;

            Storage_Contents;

                accept Deliver (Assembly : in Assembly_Type; Number : out Integer)
                do
                    if Can_Deliver (Assembly) then
                        Put_Line("Dostarczono produkt: " & Assembly_Name (Assembly) & " number " & Integer'Image (Assembly_Number (Assembly)));
                        New_Line;

                        for W in Product_Type loop
                            Storage (W) := Storage (W) - Assembly_Content_Matrix (Assembly, W);
                            In_Storage  := In_Storage - Assembly_Content_Matrix (Assembly, W);
                        end loop;

                        Number := Assembly_Number (Assembly);
                        Assembly_Number (Assembly) := Assembly_Number (Assembly) + 1;
                        delay Duration(1);
                    else
               Cant_Produce_Flag := Cant_Produce_Flag + 1;




               -- czy exception
               is_Buffer_overloaded;

               -- zakomentowany kod na rzecz modyfikacji, zamiast czyscic bufor wprowadzamy exception

                      --  if Cant_Produce_Flag >= MAX_UNSUCCESSFUL_ATTEMPTS then
                      --      Clean_buffer;
                      --      Put_Line("Zbyt wiele razy nie mozna bylo wyprodukowac zamowionego produktu, wiec browar pozbyl sie skladnikow na ktore byl maly popyt");
                      --      New_Line;
                      --      delay Duration(1);
                      --  else
                      --      Put_Line("Brakuje skladnikow do produkcji: " & Assembly_Name (Assembly) & " klient nie bedzie dluzej czekal");
                      --      New_Line;
                      --      delay Duration(1);
                      --  end if;
                    end if;
                end Deliver;
                Storage_Contents;
        end loop;
    end Buffer;

begin
    for I in 1 ..NUMBER_OF_PRODUCTS
    loop
        producer_arr (I).Start (I, 20);
    end loop;

    for J in 1 .. NUMBER_OF_CONSUMERS loop
        consumer_arr (J).Start (J, 6);
    end loop;

end Simulation;
