------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--         Copyright (C) 2015, Free Software Foundation, Inc.               --
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

package GPR.Snames is

   N : constant Name_Id := First_Name_Id;

   Name_A                                : constant Name_Id := N + 001;
   Name_B                                : constant Name_Id := N + 002;
   Name_C                                : constant Name_Id := N + 003;
   Name_D                                : constant Name_Id := N + 004;
   Name_E                                : constant Name_Id := N + 005;
   Name_F                                : constant Name_Id := N + 006;
   Name_G                                : constant Name_Id := N + 007;
   Name_H                                : constant Name_Id := N + 008;
   Name_I                                : constant Name_Id := N + 009;
   Name_J                                : constant Name_Id := N + 010;
   Name_K                                : constant Name_Id := N + 011;
   Name_L                                : constant Name_Id := N + 012;
   Name_M                                : constant Name_Id := N + 013;
   Name_N                                : constant Name_Id := N + 014;
   Name_O                                : constant Name_Id := N + 015;
   Name_P                                : constant Name_Id := N + 016;
   Name_Q                                : constant Name_Id := N + 017;
   Name_R                                : constant Name_Id := N + 018;
   Name_S                                : constant Name_Id := N + 019;
   Name_T                                : constant Name_Id := N + 020;
   Name_U                                : constant Name_Id := N + 021;
   Name_V                                : constant Name_Id := N + 022;
   Name_W                                : constant Name_Id := N + 023;
   Name_X                                : constant Name_Id := N + 024;
   Name_Y                                : constant Name_Id := N + 025;
   Name_Z                                : constant Name_Id := N + 026;
   Name_Abort                            : constant Name_Id := N + 027;
   Name_Abs                              : constant Name_Id := N + 028;
   Name_Accept                           : constant Name_Id := N + 029;
   Name_And                              : constant Name_Id := N + 030;
   Name_All                              : constant Name_Id := N + 031;
   Name_Array                            : constant Name_Id := N + 032;
   Name_At                               : constant Name_Id := N + 033;
   Name_Begin                            : constant Name_Id := N + 034;
   Name_Body                             : constant Name_Id := N + 035;
   Name_Case                             : constant Name_Id := N + 036;
   Name_Constant                         : constant Name_Id := N + 037;
   Name_Declare                          : constant Name_Id := N + 038;
   Name_Delay                            : constant Name_Id := N + 039;
   Name_Do                               : constant Name_Id := N + 040;
   Name_Else                             : constant Name_Id := N + 041;
   Name_Elsif                            : constant Name_Id := N + 042;
   Name_End                              : constant Name_Id := N + 043;
   Name_Entry                            : constant Name_Id := N + 044;
   Name_Exception                        : constant Name_Id := N + 045;
   Name_Exit                             : constant Name_Id := N + 046;
   Name_For                              : constant Name_Id := N + 047;
   Name_Function                         : constant Name_Id := N + 048;
   Name_Generic                          : constant Name_Id := N + 049;
   Name_Goto                             : constant Name_Id := N + 050;
   Name_If                               : constant Name_Id := N + 051;
   Name_In                               : constant Name_Id := N + 052;
   Name_Is                               : constant Name_Id := N + 053;
   Name_Limited                          : constant Name_Id := N + 054;
   Name_Loop                             : constant Name_Id := N + 055;
   Name_New                              : constant Name_Id := N + 056;
   Name_Not                              : constant Name_Id := N + 057;
   Name_Null                             : constant Name_Id := N + 058;
   Name_Of                               : constant Name_Id := N + 059;
   Name_Or                               : constant Name_Id := N + 060;
   Name_Others                           : constant Name_Id := N + 061;
   Name_Out                              : constant Name_Id := N + 062;
   Name_Package                          : constant Name_Id := N + 063;
   Name_Pragma                           : constant Name_Id := N + 064;
   Name_Private                          : constant Name_Id := N + 065;
   Name_Procedure                        : constant Name_Id := N + 066;
   Name_Raise                            : constant Name_Id := N + 067;
   Name_Record                           : constant Name_Id := N + 068;
   Name_Rem                              : constant Name_Id := N + 069;
   Name_Renames                          : constant Name_Id := N + 070;
   Name_Return                           : constant Name_Id := N + 071;
   Name_Reverse                          : constant Name_Id := N + 072;
   Name_Select                           : constant Name_Id := N + 073;
   Name_Separate                         : constant Name_Id := N + 074;
   Name_Subtype                          : constant Name_Id := N + 075;
   Name_Task                             : constant Name_Id := N + 076;
   Name_Terminate                        : constant Name_Id := N + 077;
   Name_Then                             : constant Name_Id := N + 078;
   Name_Type                             : constant Name_Id := N + 079;
   Name_Use                              : constant Name_Id := N + 080;
   Name_When                             : constant Name_Id := N + 081;
   Name_While                            : constant Name_Id := N + 082;
   Name_With                             : constant Name_Id := N + 083;
   Name_Xor                              : constant Name_Id := N + 084;
   Name_Access                           : constant Name_Id := N + 085;
   Name_Delta                            : constant Name_Id := N + 086;
   Name_Digits                           : constant Name_Id := N + 087;
   Name_Mod                              : constant Name_Id := N + 088;
   Name_Range                            : constant Name_Id := N + 089;
   Name_Abstract                         : constant Name_Id := N + 090;
   Name_Aliased                          : constant Name_Id := N + 091;
   Name_Protected                        : constant Name_Id := N + 092;
   Name_Until                            : constant Name_Id := N + 093;
   Name_Requeue                          : constant Name_Id := N + 094;
   Name_Tagged                           : constant Name_Id := N + 095;
   Name_Project                          : constant Name_Id := N + 096;
   Name_Extends                          : constant Name_Id := N + 097;
   Name_External                         : constant Name_Id := N + 098;
   Name_External_As_List                 : constant Name_Id := N + 099;
   Name_Interface                        : constant Name_Id := N + 100;
   Name_Overriding                       : constant Name_Id := N + 101;
   Name_Synchronized                     : constant Name_Id := N + 102;
   Name_Some                             : constant Name_Id := N + 103;
   Name_Active                           : constant Name_Id := N + 104;
   Name_Aggregate                        : constant Name_Id := N + 105;
   Name_Archive_Builder                  : constant Name_Id := N + 106;
   Name_Archive_Builder_Append_Option    : constant Name_Id := N + 107;
   Name_Archive_Indexer                  : constant Name_Id := N + 108;
   Name_Archive_Suffix                   : constant Name_Id := N + 109;
   Name_Artifacts                        : constant Name_Id := N + 110;
   Name_Artifacts_In_Exec_Dir            : constant Name_Id := N + 111;
   Name_Artifacts_In_Object_Dir          : constant Name_Id := N + 112;
   Name_Binder                           : constant Name_Id := N + 113;
   Name_Body_Suffix                      : constant Name_Id := N + 114;
   Name_Builder                          : constant Name_Id := N + 115;
   Name_Clean                            : constant Name_Id := N + 116;
   Name_Compiler                         : constant Name_Id := N + 117;
   Name_Compiler_Command                 : constant Name_Id := N + 118;
   Name_Config_Body_File_Name            : constant Name_Id := N + 119;
   Name_Config_Body_File_Name_Index      : constant Name_Id := N + 120;
   Name_Config_Body_File_Name_Pattern    : constant Name_Id := N + 121;
   Name_Config_File_Switches             : constant Name_Id := N + 122;
   Name_Config_File_Unique               : constant Name_Id := N + 123;
   Name_Config_Spec_File_Name            : constant Name_Id := N + 124;
   Name_Config_Spec_File_Name_Index      : constant Name_Id := N + 125;
   Name_Config_Spec_File_Name_Pattern    : constant Name_Id := N + 126;
   Name_Configuration                    : constant Name_Id := N + 127;
   Name_Cross_Reference                  : constant Name_Id := N + 128;
   Name_Default_Language                 : constant Name_Id := N + 129;
   Name_Default_Switches                 : constant Name_Id := N + 130;
   Name_Dependency_Driver                : constant Name_Id := N + 131;
   Name_Dependency_Kind                  : constant Name_Id := N + 132;
   Name_Dependency_Switches              : constant Name_Id := N + 133;
   Name_Driver                           : constant Name_Id := N + 134;
   Name_Excluded_Source_Dirs             : constant Name_Id := N + 135;
   Name_Excluded_Source_Files            : constant Name_Id := N + 136;
   Name_Excluded_Source_List_File        : constant Name_Id := N + 137;
   Name_Exec_Dir                         : constant Name_Id := N + 138;
   Name_Exec_Subdir                      : constant Name_Id := N + 139;
   Name_Excluded_Patterns                : constant Name_Id := N + 140;
   Name_Executable                       : constant Name_Id := N + 141;
   Name_Executable_Suffix                : constant Name_Id := N + 142;
   Name_Externally_Built                 : constant Name_Id := N + 143;
   Name_Finder                           : constant Name_Id := N + 144;
   Name_Global_Compilation_Switches      : constant Name_Id := N + 145;
   Name_Global_Configuration_Pragmas     : constant Name_Id := N + 146;
   Name_Global_Config_File               : constant Name_Id := N + 147;
   Name_Gnatls                           : constant Name_Id := N + 148;
   Name_Gnatstub                         : constant Name_Id := N + 149;
   Name_Gnu                              : constant Name_Id := N + 150;
   Name_Ide                              : constant Name_Id := N + 151;
   Name_Ignore_Source_Sub_Dirs           : constant Name_Id := N + 152;
   Name_Implementation                   : constant Name_Id := N + 153;
   Name_Implementation_Exceptions        : constant Name_Id := N + 154;
   Name_Implementation_Suffix            : constant Name_Id := N + 155;
   Name_Included_Artifact_Patterns       : constant Name_Id := N + 156;
   Name_Included_Patterns                : constant Name_Id := N + 157;
   Name_Include_Switches                 : constant Name_Id := N + 158;
   Name_Include_Path                     : constant Name_Id := N + 159;
   Name_Include_Path_File                : constant Name_Id := N + 160;
   Name_Inherit_Source_Path              : constant Name_Id := N + 161;
   Name_Install                          : constant Name_Id := N + 162;
   Name_Languages                        : constant Name_Id := N + 163;
   Name_Language_Kind                    : constant Name_Id := N + 164;
   Name_Leading_Library_Options          : constant Name_Id := N + 165;
   Name_Leading_Required_Switches        : constant Name_Id := N + 166;
   Name_Leading_Switches                 : constant Name_Id := N + 167;
   Name_Lib_Subdir                       : constant Name_Id := N + 168;
   Name_Link_Lib_Subdir                  : constant Name_Id := N + 169;
   Name_Library                          : constant Name_Id := N + 170;
   Name_Library_Ali_Dir                  : constant Name_Id := N + 171;
   Name_Library_Auto_Init                : constant Name_Id := N + 172;
   Name_Library_Auto_Init_Supported      : constant Name_Id := N + 173;
   Name_Library_Builder                  : constant Name_Id := N + 174;
   Name_Library_Dir                      : constant Name_Id := N + 175;
   Name_Library_GCC                      : constant Name_Id := N + 176;
   Name_Library_Install_Name_Option      : constant Name_Id := N + 177;
   Name_Library_Interface                : constant Name_Id := N + 178;
   Name_Library_Kind                     : constant Name_Id := N + 179;
   Name_Library_Name                     : constant Name_Id := N + 180;
   Name_Library_Major_Minor_Id_Supported : constant Name_Id := N + 181;
   Name_Library_Options                  : constant Name_Id := N + 182;
   Name_Library_Partial_Linker           : constant Name_Id := N + 183;
   Name_Library_Reference_Symbol_File    : constant Name_Id := N + 184;
   Name_Library_Rpath_Options            : constant Name_Id := N + 185;
   Name_Library_Standalone               : constant Name_Id := N + 186;
   Name_Library_Encapsulated_Options     : constant Name_Id := N + 187;
   Name_Library_Encapsulated_Supported   : constant Name_Id := N + 188;
   Name_Library_Src_Dir                  : constant Name_Id := N + 189;
   Name_Library_Support                  : constant Name_Id := N + 190;
   Name_Library_Symbol_File              : constant Name_Id := N + 191;
   Name_Library_Symbol_Policy            : constant Name_Id := N + 192;
   Name_Library_Version                  : constant Name_Id := N + 193;
   Name_Library_Version_Switches         : constant Name_Id := N + 194;
   Name_Linker                           : constant Name_Id := N + 195;
   Name_Linker_Executable_Option         : constant Name_Id := N + 196;
   Name_Linker_Lib_Dir_Option            : constant Name_Id := N + 197;
   Name_Linker_Lib_Name_Option           : constant Name_Id := N + 198;
   Name_Local_Config_File                : constant Name_Id := N + 199;
   Name_Local_Configuration_Pragmas      : constant Name_Id := N + 200;
   Name_Locally_Removed_Files            : constant Name_Id := N + 201;
   Name_Map_File_Option                  : constant Name_Id := N + 202;
   Name_Mapping_File_Switches            : constant Name_Id := N + 203;
   Name_Mapping_Spec_Suffix              : constant Name_Id := N + 204;
   Name_Mapping_Body_Suffix              : constant Name_Id := N + 205;
   Name_Max_Command_Line_Length          : constant Name_Id := N + 206;
   Name_Metrics                          : constant Name_Id := N + 207;
   Name_Multi_Unit_Object_Separator      : constant Name_Id := N + 208;
   Name_Multi_Unit_Switches              : constant Name_Id := N + 209;
   Name_Naming                           : constant Name_Id := N + 210;
   Name_None                             : constant Name_Id := N + 211;
   Name_Object_Artifact_Extensions       : constant Name_Id := N + 212;
   Name_Object_File_Suffix               : constant Name_Id := N + 213;
   Name_Object_File_Switches             : constant Name_Id := N + 214;
   Name_Object_Generated                 : constant Name_Id := N + 215;
   Name_Object_List                      : constant Name_Id := N + 216;
   Name_Object_Path_Switches             : constant Name_Id := N + 217;
   Name_Objects_Linked                   : constant Name_Id := N + 218;
   Name_Objects_Path                     : constant Name_Id := N + 219;
   Name_Objects_Path_File                : constant Name_Id := N + 220;
   Name_Object_Dir                       : constant Name_Id := N + 221;
   Name_Option_List                      : constant Name_Id := N + 222;
   Name_Path_Syntax                      : constant Name_Id := N + 223;
   Name_Pic_Option                       : constant Name_Id := N + 224;
   Name_Pretty_Printer                   : constant Name_Id := N + 225;
   Name_Prefix                           : constant Name_Id := N + 226;
   Name_Project_Dir                      : constant Name_Id := N + 227;
   Name_Project_Files                    : constant Name_Id := N + 228;
   Name_Project_Path                     : constant Name_Id := N + 229;
   Name_Project_Subdir                   : constant Name_Id := N + 230;
   Name_Remote                           : constant Name_Id := N + 231;
   Name_Response_File_Format             : constant Name_Id := N + 232;
   Name_Response_File_Switches           : constant Name_Id := N + 233;
   Name_Root_Dir                         : constant Name_Id := N + 234;
   Name_Roots                            : constant Name_Id := N + 235;
   Name_Required_Switches                : constant Name_Id := N + 236;
   Name_Run_Path_Option                  : constant Name_Id := N + 237;
   Name_Run_Path_Origin                  : constant Name_Id := N + 238;
   Name_Separate_Run_Path_Options        : constant Name_Id := N + 239;
   Name_Shared_Library_Minimum_Switches  : constant Name_Id := N + 240;
   Name_Shared_Library_Prefix            : constant Name_Id := N + 241;
   Name_Shared_Library_Suffix            : constant Name_Id := N + 242;
   Name_Separate_Suffix                  : constant Name_Id := N + 243;
   Name_Source_Artifact_Extensions       : constant Name_Id := N + 244;
   Name_Source_Dirs                      : constant Name_Id := N + 245;
   Name_Source_File_Switches             : constant Name_Id := N + 246;
   Name_Source_Files                     : constant Name_Id := N + 247;
   Name_Source_List_File                 : constant Name_Id := N + 248;
   Name_Sources_Subdir                   : constant Name_Id := N + 249;
   Name_Spec                             : constant Name_Id := N + 250;
   Name_Spec_Suffix                      : constant Name_Id := N + 251;
   Name_Specification                    : constant Name_Id := N + 252;
   Name_Specification_Exceptions         : constant Name_Id := N + 253;
   Name_Specification_Suffix             : constant Name_Id := N + 254;
   Name_Stack                            : constant Name_Id := N + 255;
   Name_Switches                         : constant Name_Id := N + 256;
   Name_Symbolic_Link_Supported          : constant Name_Id := N + 257;
   Name_Toolchain_Description            : constant Name_Id := N + 258;
   Name_Toolchain_Version                : constant Name_Id := N + 259;
   Name_Trailing_Required_Switches       : constant Name_Id := N + 260;
   Name_Trailing_Switches                : constant Name_Id := N + 261;
   Name_Runtime_Library_Dir              : constant Name_Id := N + 262;
   Name_Runtime_Source_Dir               : constant Name_Id := N + 263;
   Name_Ada                              : constant Name_Id := N + 264;
   Name_Interfaces                       : constant Name_Id := N + 265;
   Name_Main                             : constant Name_Id := N + 266;
   Name_Target                           : constant Name_Id := N + 267;
   Name_Casing                           : constant Name_Id := N + 268;
   Name_Dot_Replacement                  : constant Name_Id := N + 269;
   Name_Standard                         : constant Name_Id := N + 270;
   Name_Name                             : constant Name_Id := N + 271;
   Name_Linker_Options                   : constant Name_Id := N + 272;
   Name_Runtime                          : constant Name_Id := N + 273;
   Name_Mode                             : constant Name_Id := N + 274;
   Name_Install_Name                     : constant Name_Id := N + 275;

   subtype Reserved_Ada_95 is Name_Id
      range Name_Abort .. Name_Tagged;
   subtype Reserved_Ada_Project is Name_Id
      range Name_Abort .. Name_External_As_List;
   subtype Reserved_Ada_Other is Name_Id
      range Name_Interface .. Name_Some;

   procedure Initialize;

end GPR.Snames;
