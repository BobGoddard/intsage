--  Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
--  This file is free software: you may copy, redistribute and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 2 of the License, or (at your
--  option) any later version.
--
--  This file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with GNAT.Traceback;
with System;
with IntSage_Map;
with SNMP_Defines; use SNMP_Defines;

package SNMP_Code is
   task type SNMP_Code_Loop is
      entry Start;
      entry Kill_Me;
   end SNMP_Code_Loop;

private
   type OID_List_Record is record
      OID  : SNMP_Defines.OID_Max_Array;
      Len  : Interfaces.C.size_t;
      Desc : String (1 .. 3);
   end record;

   type TS_Record is record
      TV_Sec    : Interfaces.C.long;
      TV_NSec   : Interfaces.C.long;
   end record;

   CLOCK_REALTIME  : constant Interfaces.C.int := 0;
   Session_Start   : aliased  SNMP_Defines.SNMP_Session;
   Session_Opaque  : access   SNMP_Defines.SNMP_Session;
   PDU_Details     : access   SNMP_Defines.SNMP_PDU;
   Var_List        : access   SNMP_Defines.variable_list;
   SNMP_Agent_Name : constant Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("intsage");
   Password        : constant Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("Ch1cken1");
   Data_Map_Int    :          IntSage_Map.Int_Sage_Map.Map;
   Data_Map_ADSL   :          IntSage_Map.ADSL_Sage_Map.Map;
   Trace           : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length          : Natural;

   procedure Clock_Get_Time (Clk_ID : Interfaces.C.int; TS : System.Address);
   pragma Import (C, Clock_Get_Time, "clock_gettime");
   procedure Populate_ADSL;
   procedure Populate_ExInt;
   procedure Populate_Int;
end SNMP_Code;
