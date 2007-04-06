with Interfaces.C.Strings; use Interfaces.C.Strings;
package Animals is
   --  -----------------------------------------------------------------------
   type Carnivore is limited interface;
   function Number_Of_Teeth (X : Carnivore) return Natural is abstract;
   pragma Convention (CPP, Number_Of_Teeth); --  Required by AI-430

   --  -----------------------------------------------------------------------
   type Domestic is limited interface;
   procedure Set_Owner (X : in out Domestic; Name : Chars_Ptr) is abstract;
   pragma Convention (CPP, Set_Owner);

   --  -----------------------------------------------------------------------
   type Animal is tagged limited record
      Age : Natural := 0;
   end record;
   pragma CPP_Class (Animal);

   procedure Set_Age (X : in out Animal; Age : Natural);
   pragma Import (CPP, Set_Age);

   function Age (X : Animal) return Natural;
   pragma Import (CPP, Age);

   --  -----------------------------------------------------------------------
   type Dog is new Animal and Carnivore and Domestic with record
      Tooth_Count : Natural;
      Owner       : String (1 .. 30);
   end record;
   pragma CPP_Class (Dog);

   function Number_Of_Teeth (A : Dog) return Natural;
   pragma Import (CPP, Number_Of_Teeth);

   procedure Set_Owner (A : in out Dog; Name : Chars_Ptr);
   pragma Import (CPP, Set_Owner);

   function New_Dog return Dog'Class;
   pragma CPP_Constructor (New_Dog);
   pragma Import (CPP, New_Dog, "_ZN3DogC2Ev");

   --  -----------------------------------------------------------------------
   --  Example of a type derivation defined in the Ada side that inherites
   --  all the dispatching primitives of the ancestor from the C++ side.

   type Vaccinated_Dog is new Dog with null record;
   function Vaccination_Expired (A : Vaccinated_Dog) return Boolean;
   pragma Convention (CPP, Vaccination_Expired); 
end Animals;
