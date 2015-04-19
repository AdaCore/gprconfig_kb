------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This packages contains global variables and routines common to error
--  reporting packages, including Errout and Prj.Err.

with GNAT.Table;

with GPR.Osint; use GPR.Osint;

package GPR.Erroutc is

   Continuation : Boolean := False;
   --  Indicates if current message is a continuation. Initialized from the
   --  Msg_Cont parameter in Error_Msg_Internal and then set True if a \
   --  insertion character is encountered.

   Has_Double_Exclam : Boolean := False;
   --  Set true to indicate that the current message contains the insertion
   --  sequence !! (force warnings even in non-main unit source files).

   Is_Serious_Error : Boolean := False;
   --  Set True for a serious error (i.e. any message that is not a warning
   --  or style message, and that does not contain a | insertion character).

   Is_Unconditional_Msg : Boolean := False;
   --  Set True to indicate that the current message contains the insertion
   --  character ! and is thus to be treated as an unconditional message.

   Is_Warning_Msg : Boolean := False;
   --  Set True to indicate if current message is warning message (contains ?
   --  or contains < and Error_Msg_Warn is True.

   Is_Info_Msg : Boolean := False;
   --  Set True to indicate that the current message starts with the characters
   --  "info: " and is to be treated as an information message. This string
   --  will be prepended to the message and all its continuations.

   Warning_Msg_Char : Character;
   --  Warning character, valid only if Is_Warning_Msg is True
   --    ' '      -- ?   or <   appeared on its own in message
   --    '?'      -- ??  or <<  appeared in message
   --    'x'      -- ?x? or <x< appeared in message (x = a .. z)
   --    'X'      -- ?X? or <X< appeared in message (X = A .. Z)
   --    '*'      -- ?*? or <*< appeared in message
   --    '$'      -- ?$? or <$< appeared in message
   --  In the case of the < sequences, this is set only if the message is
   --  actually a warning, i.e. if Error_Msg_Warn is True

   Kill_Message : Boolean := False;
   --  A flag used to kill weird messages (e.g. those containing uninterpreted
   --  implicit type references) if we have already seen at least one message
   --  already. The idea is that we hope the weird message is a junk cascaded
   --  message that should be suppressed.

   Last_Killed : Boolean := False;
   --  Set True if the most recently posted non-continuation message was
   --  killed. This is used to determine the processing of any continuation
   --  messages that follow.

   Manual_Quote_Mode : Boolean := False;
   --  Set True in manual quotation mode

   Max_Msg_Length : constant := 1024 + 2 * Int (Column_Number'Last);
   --  Maximum length of error message. The addition of 2 * Column_Number'Last
   --  ensures that two insertion tokens of maximum length can be accommodated.
   --  The value of 1024 is an arbitrary value that should be more than long
   --  enough to accommodate any reasonable message (and for that matter, some
   --  pretty unreasonable messages).

   Msg_Buffer : String (1 .. Max_Msg_Length);
   --  Buffer used to prepare error messages

   Msglen : Integer := 0;
   --  Number of characters currently stored in the message buffer

   Suppress_Message : Boolean;
   --  A flag used to suppress certain obviously redundant messages (i.e.
   --  those referring to a node whose type is Any_Type). This suppression
   --  is effective only if All_Errors_Mode is off.

   ----------------------------
   -- Message ID Definitions --
   ----------------------------

   type Error_Msg_Id is new Int;
   --  A type used to represent specific error messages. Used by the clients
   --  of this package only in the context of the Get_Error_Id and
   --  Change_Error_Text subprograms.

   No_Error_Msg : constant Error_Msg_Id := 0;
   --  A constant which is different from any value returned by Get_Error_Id.
   --  Typically used by a client to indicate absence of a saved Id value.

   Cur_Msg : Error_Msg_Id := No_Error_Msg;
   --  Id of most recently posted error message

   function Get_Msg_Id return Error_Msg_Id;
   --  Returns the Id of the message most recently posted using one of the
   --  Error_Msg routines.

   function Get_Location (E : Error_Msg_Id) return Source_Ptr;
   --  Returns the flag location of the error message with the given id E

   -----------------------------------
   -- Error Message Data Structures --
   -----------------------------------

   --  The error messages are stored as a linked list of error message objects
   --  sorted into ascending order by the source location (Sloc). Each object
   --  records the text of the message and its source location.

   --  The following record type and table are used to represent error
   --  messages, with one entry in the table being allocated for each message.

   type Error_Msg_Object is record
      Text : String_Access;
      --  Text of error message, fully expanded with all insertions

      Next : Error_Msg_Id;
      --  Pointer to next message in error chain. A value of No_Error_Msg
      --  indicates the end of the chain.

      Prev : Error_Msg_Id;
      --  Pointer to previous message in error chain. Only set during the
      --  Finalize procedure. A value of No_Error_Msg indicates the first
      --  message in the chain.

      Sfile : Source_File_Index;
      --  Source table index of source file. In the case of an error that
      --  refers to a template, always references the original template
      --  not an instantiation copy.

      Sptr : Source_Ptr;
      --  Flag pointer. In the case of an error that refers to a template,
      --  always references the original template, not an instantiation copy.
      --  This value is the actual place in the source that the error message
      --  will be posted. Note that an error placed on an instantiation will
      --  have Sptr pointing to the instantiation point.

      Optr : Source_Ptr;
      --  Flag location used in the call to post the error. This is normally
      --  the same as Sptr, except when an error is posted on a particular
      --  instantiation of a generic. In such a case, Sptr will point to
      --  the original source location of the instantiation itself, but
      --  Optr will point to the template location (more accurately to the
      --  template copy in the instantiation copy corresponding to the
      --  instantiation referenced by Sptr).

      Line : Line_Number;
      --  Line number for error message

      Col : Column_Number;
      --  Column number for error message

      Warn : Boolean;
      --  True if warning message

      Info : Boolean;
      --  True if info message

      Warn_Err : Boolean;
      --  True if this is a warning message which is to be treated as an error
      --  as a result of a match with a Warning_As_Error pragma.

      Warn_Chr : Character;
      --  Warning character (note: set even if Warning_Doc_Switch is False)
      --    ' '      -- ?   or <   appeared on its own in message
      --    '?'      -- ??  or <<  appeared in message
      --    'x'      -- ?x? or <x< appeared in message (x = a .. z)
      --    'X'      -- ?X? or <X< appeared in message (X = A .. Z)
      --    '*'      -- ?*? or <*< appeared in message
      --    '$'      -- ?$? or <$< appeared in message
      --  In the case of the < sequences, this is set only if the message is
      --  actually a warning, i.e. if Error_Msg_Warn is True

      Serious : Boolean;
      --  True if serious error message (not a warning and no | character)

      Uncond : Boolean;
      --  True if unconditional message (i.e. insertion character ! appeared)

      Msg_Cont : Boolean;
      --  This is used for logical messages that are composed of multiple
      --  individual messages. For messages that are not part of such a
      --  group, or that are the first message in such a group. Msg_Cont
      --  is set to False. For subsequent messages in a group, Msg_Cont
      --  is set to True. This is used to make sure that such a group of
      --  messages is either suppressed or retained as a group (e.g. in
      --  the circuit that deletes identical messages).

      Deleted : Boolean;
      --  If this flag is set, the message is not printed. This is used
      --  in the circuit for deleting duplicate/redundant error messages.
   end record;

   package Errors is new GNAT.Table (
     Table_Component_Type => Error_Msg_Object,
     Table_Index_Type     => Error_Msg_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 200,
     Table_Increment      => 200);

   First_Error_Msg : Error_Msg_Id;
   --  The list of error messages, i.e. the first entry on the list of error
   --  messages. This is not the same as the physically first entry in the
   --  error message table, since messages are not always inserted in sequence.

   Last_Error_Msg : Error_Msg_Id;
   --  The last entry on the list of error messages. Note: this is not the same
   --  as the physically last entry in the error message table, since messages
   --  are not always inserted in sequence.

   Error_Msg_Name_1 : Name_Id := No_Name;
   Error_Msg_Name_2 : Name_Id := No_Name;

   Error_Msg_File_1 : File_Name_Type := No_File;
   Error_Msg_File_2 : File_Name_Type := No_File;

   Error_Msg_Warn : Boolean := False;

   Error_Msg_String : String (1 .. 4096);
   Error_Msg_Strlen : Natural;
   --  Used if current message contains a ~ insertion character to indicate
   --  insertion of the string Error_Msg_String (1 .. Error_Msg_Strlen).

   --------------------------
   -- Warning Mode Control --
   --------------------------

   --  Pragma Warnings allows warnings to be turned off for a specified region
   --  of code, and the following tables are the data structures used to keep
   --  track of these regions.

   --  The first table is used for the basic command line control, and for the
   --  forms of Warning with a single ON or OFF parameter.

   --  It contains pairs of source locations, the first being the start
   --  location for a warnings off region, and the second being the end
   --  location. When a pragma Warnings (Off) is encountered, a new entry is
   --  established extending from the location of the pragma to the end of the
   --  current source file. A subsequent pragma Warnings (On) adjusts the end
   --  point of this entry appropriately.

   --  If all warnings are suppressed by command switch, then there is a dummy
   --  entry (put there by Errout.Initialize) at the start of the table which
   --  covers all possible Source_Ptr values. Note that the source pointer
   --  values in this table always reference the original template, not an
   --  instantiation copy, in the generic case.

   --  Reason is the reason from the pragma Warnings (Off,..) or the null
   --  string if no reason parameter is given.

   type Warnings_Entry is record
      Start  : Source_Ptr;
      Stop   : Source_Ptr;
      Reason : Name_Id;
   end record;

   package Warnings is new GNAT.Table (
     Table_Component_Type => Warnings_Entry,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200);

   -----------------
   -- Subprograms --
   -----------------

   function Compilation_Errors return Boolean;
   --  Returns true if errors have been detected, or warnings in -gnatwe
   --  (treat warnings as errors) mode.

   procedure Check_Duplicate_Message (M1, M2 : Error_Msg_Id);
   --  This function is passed the Id values of two error messages. If either
   --  M1 or M2 is a continuation message, or is already deleted, the call is
   --  ignored. Otherwise a check is made to see if M1 and M2 are duplicated or
   --  redundant. If so, the message to be deleted and all its continuations
   --  are marked with the Deleted flag set to True.

   function Is_Start_Of_Wide_Char
     (S : Source_Buffer_Ptr;
      P : Source_Ptr) return Boolean;
   --  Determines if S (P) is the start of a wide character sequence

   procedure Output_Error_Msgs (E : in out Error_Msg_Id);
   --  Output source line, error flag, and text of stored error message and all
   --  subsequent messages for the same line and unit. On return E is set to be
   --  one higher than the last message output.

   procedure Output_Line_Number (L : Line_Number);
   --  Output a line number as six digits (with leading zeroes suppressed),
   --  followed by a period and a blank (note that this is 8 characters which
   --  means that tabs in the source line will not get messed up). Line numbers
   --  that match or are less than the last Source_Reference pragma are listed
   --  as all blanks, avoiding output of junk line numbers.

   procedure Output_Msg_Text (E : Error_Msg_Id);
   --  Outputs characters of text in the text of the error message E. Note that
   --  no end of line is output, the caller is responsible for adding the end
   --  of line. If Error_Msg_Line_Length is non-zero, this is the routine that
   --  splits the line generating multiple lines of output, and in this case
   --  the last line has no terminating end of line character.

   procedure Set_Msg_Char (C : Character);
   --  Add a single character to the current message. This routine does not
   --  check for special insertion characters (they are just treated as text
   --  characters if they occur).

   procedure Set_Msg_Insertion_File_Name;
   --  Handle file name insertion (left brace insertion character)

   procedure Set_Msg_Insertion_Name_Literal;

   procedure Set_Msg_Insertion_Name;
   --  Handle name insertion (% insertion character)

   procedure Set_Msg_Insertion_Reserved_Name;
   --  Handle insertion of reserved word name (* insertion character)

   procedure Set_Msg_Insertion_Reserved_Word
     (Text : String;
      J    : in out Integer);
   --  Handle reserved word insertion (upper case letters). The Text argument
   --  is the current error message input text, and J is an index which on
   --  entry points to the first character of the reserved word, and on exit
   --  points past the last character of the reserved word. Note that RM and
   --  SPARK are treated specially and not considered to be keywords.

   procedure Set_Msg_Str (Text : String);
   --  Add a sequence of characters to the current message. This routine does
   --  not check for special insertion characters (they are just treated as
   --  text characters if they occur). It does perform the transformation of
   --  the special strings _xxx (xxx = Pre/Post/Type_Invariant) to xxx'Class.

end GPR.Erroutc;
