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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded;
with GNAT.Calendar;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Traces.Syslog;
with List_Handlers;
with SQL_Helpers;
with Structures;

package body SNMP_Code is
   procedure Populate_ADSL is
      C_ADSL           : IntSage_Map.ADSL_Cursor;
      Data_ADSL_Struct : Structures.ADSL_Int_Face_Type;
   begin
      if not Data_Map_ADSL.Contains (Var_List.all.Name.all (14)) then
         declare
            AIS : Structures.ADSL_Int_Face_Type;
         begin
            Data_Map_ADSL.Insert (Var_List.all.Name.all (14), AIS);
         exception
            when E : others =>
               GNAT.Traceback.Call_Chain (Trace, Length);
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                                GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end;
      end if;

      C_ADSL := Data_Map_ADSL.Find (Var_List.all.Name.all (14));
      Data_ADSL_Struct := Data_Map_ADSL.Element (Var_List.all.Name.all (14));
      Data_ADSL_Struct.ifIndex := Interfaces.C.long (Var_List.all.Name.all (14));
      case Var_List.all.Name.all (11) is
         when 1 =>
            case Var_List.all.Name.all (13) is
               when 1 => Data_ADSL_Struct.adslLineCoding := Var_List.all.Val.integer.all;
               when 2 => Data_ADSL_Struct.adslLineType   := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when 2 =>
            case Var_List.all.Name.all (13) is
               when 2 => Data_ADSL_Struct.adslAtucInvVendorID        := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Var_List.all.Val.string));
               when 3 => Data_ADSL_Struct.adslAtucInvVersionNumber   := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Var_List.all.Val.string));
               when 4 => Data_ADSL_Struct.adslAtucCurrSnrMgn         := Var_List.all.Val.integer.all;
               when 5 => Data_ADSL_Struct.adslAtucCurrAtn            := Var_List.all.Val.integer.all;
               when 7 => Data_ADSL_Struct.adslAtucCurrOutputPwr      := Var_List.all.Val.integer.all;
               when 8 => Data_ADSL_Struct.adslAtucCurrAttainableRate := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when 3 =>
            case Var_List.all.Name.all (13) is
               when 1 => Data_ADSL_Struct.adslAturInvSerialNumber    := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Var_List.all.Val.string));
               when 2 => Data_ADSL_Struct.adslAturInvVendorID        := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Var_List.all.Val.string));
               when 3 => Data_ADSL_Struct.adslAturInvVersionNumber   := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Var_List.all.Val.string));
               when 4 => Data_ADSL_Struct.adslAturCurrSnrMgn         := Var_List.all.Val.integer.all;
               when 5 => Data_ADSL_Struct.adslAturCurrAtn            := Var_List.all.Val.integer.all;
               when 7 => Data_ADSL_Struct.adslAturCurrOutputPwr      := Var_List.all.Val.integer.all;
               when 8 => Data_ADSL_Struct.adslAturCurrAttainableRate := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when 4 =>
            case Var_List.all.Name.all (13) is
               when 2 => Data_ADSL_Struct.adslAtucChanCurrTxRate := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when 5 =>
            case Var_List.all.Name.all (13) is
               when 2 => Data_ADSL_Struct.adslAturChanCurrTxRate := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when 6 =>
            case Var_List.all.Name.all (13) is
               when  1 => Data_ADSL_Struct.adslAtucPerfLofs            := Var_List.all.Val.integer.all;
               when  2 => Data_ADSL_Struct.adslAtucPerfLoss            := Var_List.all.Val.integer.all;
               when  4 => Data_ADSL_Struct.adslAtucPerfLprs            := Var_List.all.Val.integer.all;
               when  5 => Data_ADSL_Struct.adslAtucPerfESs             := Var_List.all.Val.integer.all;
               when  6 => Data_ADSL_Struct.adslAtucPerfInits           := Var_List.all.Val.integer.all;
               when 10  => Data_ADSL_Struct.adslAtucPerfCurr15MinLofs  := Var_List.all.Val.integer.all;
               when 11  => Data_ADSL_Struct.adslAtucPerfCurr15MinLoss  := Var_List.all.Val.integer.all;
               when 13  => Data_ADSL_Struct.adslAtucPerfCurr15MinLprs  := Var_List.all.Val.integer.all;
               when 14  => Data_ADSL_Struct.adslAtucPerfCurr15MinESs   := Var_List.all.Val.integer.all;
               when 15  => Data_ADSL_Struct.adslAtucPerfCurr15MinInits := Var_List.all.Val.integer.all;
               when 17  => Data_ADSL_Struct.adslAtucPerfCurr1DayLofs   := Var_List.all.Val.integer.all;
               when 18  => Data_ADSL_Struct.adslAtucPerfCurr1DayLoss   := Var_List.all.Val.integer.all;
               when 20  => Data_ADSL_Struct.adslAtucPerfCurr1DayLprs   := Var_List.all.Val.integer.all;
               when 21  => Data_ADSL_Struct.adslAtucPerfCurr1DayESs    := Var_List.all.Val.integer.all;
               when 22  => Data_ADSL_Struct.adslAtucPerfCurr1DayInits  := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when 7 =>
            case Var_List.all.Name.all (13) is
               when 1  => Data_ADSL_Struct.adslAturPerfLofs           := Var_List.all.Val.integer.all;
               when 2  => Data_ADSL_Struct.adslAturPerfLoss           := Var_List.all.Val.integer.all;
               when 3  => Data_ADSL_Struct.adslAturPerfLprs           := Var_List.all.Val.integer.all;
               when 4  => Data_ADSL_Struct.adslAturPerfESs            := Var_List.all.Val.integer.all;
               when 7  => Data_ADSL_Struct.adslAturPerfCurr15MinLofs  := Var_List.all.Val.integer.all;
               when 8  => Data_ADSL_Struct.adslAturPerfCurr15MinLoss  := Var_List.all.Val.integer.all;
               when 10  => Data_ADSL_Struct.adslAturPerfCurr15MinLprs := Var_List.all.Val.integer.all;
               when 11  => Data_ADSL_Struct.adslAturPerfCurr15MinESs  := Var_List.all.Val.integer.all;
               when 13  => Data_ADSL_Struct.adslAturPerfCurr1DayLofs  := Var_List.all.Val.integer.all;
               when 14  => Data_ADSL_Struct.adslAturPerfCurr1DayLoss  := Var_List.all.Val.integer.all;
               when 15  => Data_ADSL_Struct.adslAturPerfCurr1DayLprs  := Var_List.all.Val.integer.all;
               when 16  => Data_ADSL_Struct.adslAturPerfCurr1DayESs   := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when 10 =>
            case Var_List.all.Name.all (13) is
               when  3  => Data_ADSL_Struct.adslAtucChanCorrectedBlks              := Var_List.all.Val.integer.all;
               when  4  => Data_ADSL_Struct.adslAtucChanUncorrectBlks              := Var_List.all.Val.integer.all;
               when 10  => Data_ADSL_Struct.adslAtucChanPerfCurr15MinCorrectedBlks := Var_List.all.Val.integer.all;
               when 11  => Data_ADSL_Struct.adslAtucChanPerfCurr15MinUncorrectBlks := Var_List.all.Val.integer.all;
               when 15  => Data_ADSL_Struct.adslAtucChanPerfCurr1DayCorrectedBlks  := Var_List.all.Val.integer.all;
               when 16  => Data_ADSL_Struct.adslAtucChanPerfCurr1DayUncorrectBlks  := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when 11 =>
            case Var_List.all.Name.all (13) is
               when  3 => Data_ADSL_Struct.adslAturChanCorrectedBlks               := Var_List.all.Val.integer.all;
               when  4 => Data_ADSL_Struct.adslAturChanUncorrectBlks               := Var_List.all.Val.integer.all;
               when 10  => Data_ADSL_Struct.adslAturChanPerfCurr15MinCorrectedBlks := Var_List.all.Val.integer.all;
               when 11  => Data_ADSL_Struct.adslAturChanPerfCurr15MinUncorrectBlks := Var_List.all.Val.integer.all;
               when 15  => Data_ADSL_Struct.adslAturChanPerfCurr1DayCorrectedBlks  := Var_List.all.Val.integer.all;
               when 16  => Data_ADSL_Struct.adslAturChanPerfCurr1DayUncorrectBlks  := Var_List.all.Val.integer.all;
               when others => null;
            end case;
         when others => null;
      end case;

      Data_Map_ADSL.Replace_Element (C_ADSL, Data_ADSL_Struct);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Populate_ADSL;

   procedure Populate_ExInt is
      C_Int           : IntSage_Map.Int_Cursor;
      Data_Int_Struct : Structures.Int_Face_Type;
   begin
      if not Data_Map_Int.Contains (Var_List.all.Name.all (12)) then
         declare
            DIS : Structures.Int_Face_Type;
         begin
            Data_Map_Int.Insert (Var_List.all.Name.all (12), DIS);
         exception
            when E : others =>
               GNAT.Traceback.Call_Chain (Trace, Length);
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                                GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end;
      end if;

      C_Int := Data_Map_Int.Find (Var_List.all.Name.all (12));
      Data_Int_Struct := Data_Map_Int.Element (Var_List.all.Name.all (12));
      case Var_List.all.Name.all (11) is
         when  1 => Data_Int_Struct.ifName                     := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Var_List.all.Val.string));
         when  2 => Data_Int_Struct.ifInMulticastPkts          := Var_List.all.Val.integer.all;
         when  3 => Data_Int_Struct.ifInBroadcastPkts          := Var_List.all.Val.integer.all;
         when  4 => Data_Int_Struct.ifOutMulticastPkts         := Var_List.all.Val.integer.all;
         when  5 => Data_Int_Struct.ifOutBroadcastPkts         := Var_List.all.Val.integer.all;
         when  6 => Data_Int_Struct.ifHCInOctets               := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.High), 32) + Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.Low);
         when  7 => Data_Int_Struct.ifHCInUcastPkts            := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.High), 32) + Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.Low);
         when  8 => Data_Int_Struct.ifHCInMulticastPkts        := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.High), 32) + Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.Low);
         when  9 => Data_Int_Struct.ifHCInBroadcastPkts        := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.High), 32) + Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.Low);
         when 10 => Data_Int_Struct.ifHCOutOctets              := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.High), 32) + Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.Low);
         when 11 => Data_Int_Struct.ifHCOutUcastPkts           := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.High), 32) + Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.Low);
         when 12 => Data_Int_Struct.ifHCOutMulticastPkts       := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.High), 32) + Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.Low);
         when 13 => Data_Int_Struct.ifHCOutBroadcastPkts       := Interfaces.Shift_Left (Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.High), 32) + Interfaces.Unsigned_64 (Var_List.all.Val.the_counter64.all.Low);
         when 19 => Data_Int_Struct.ifCounterDiscontinuityTime := Var_List.all.Val.integer.all;
         when others => null;
      end case;

      Data_Map_Int.Replace_Element (C_Int, Data_Int_Struct);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Populate_ExInt;

   procedure Populate_Int is
      C_Int           : IntSage_Map.Int_Cursor;
      Data_Int_Struct : Structures.Int_Face_Type;
   begin
      if not Data_Map_Int.Contains (Var_List.all.Name.all (11)) then
         declare
            DIS : Structures.Int_Face_Type;
         begin
            Data_Map_Int.Insert (Var_List.all.Name.all (11), DIS);
         exception
            when E : others =>
               GNAT.Traceback.Call_Chain (Trace, Length);
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                                GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end;
      end if;

      C_Int := Data_Map_Int.Find (Var_List.all.Name.all (11));
      Data_Int_Struct := Data_Map_Int.Element (Var_List.all.Name.all (11));
      case Var_List.all.Name.all (10) is
         when  1 => Data_Int_Struct.ifIndex           := Var_List.all.Val.integer.all;
         when  2 => Data_Int_Struct.ifDesc            := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Var_List.all.Val.string));
         when  7 => Data_Int_Struct.ifAdminStatus     := Var_List.all.Val.integer.all;
         when  8 => Data_Int_Struct.ifOperStatus      := Var_List.all.Val.integer.all;
         when  9 => Data_Int_Struct.ifLastChange      := Var_List.all.Val.integer.all;
         when 10 => Data_Int_Struct.ifInOctets        := Var_List.all.Val.integer.all;
         when 13 => Data_Int_Struct.ifInDiscards      := Var_List.all.Val.integer.all;
         when 14 => Data_Int_Struct.ifInErrors        := Var_List.all.Val.integer.all;
         when 15 => Data_Int_Struct.ifInUnknownProtos := Var_List.all.Val.integer.all;
         when 16 => Data_Int_Struct.ifOutOctets       := Var_List.all.Val.integer.all;
         when 19 => Data_Int_Struct.ifOutDiscards     := Var_List.all.Val.integer.all;
         when 20 => Data_Int_Struct.ifOutErrors       := Var_List.all.Val.integer.all;
         when others => null;
      end case;

      Data_Map_Int.Replace_Element (C_Int, Data_Int_Struct);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Populate_Int;

   task body SNMP_Code_Loop is
      Tmp_Start_Sec        : String := "          ";
      Tmp_Start_NSec       : String := "         ";
      Tmp_End_Sec          : String := "          ";
      Tmp_End_NSec         : String := "         ";
      Target_Int_OID       : constant SNMP_Defines.OID_Max_Array := (1, 3, 6, 1, 2, 1,  2,  2,  1,             others => 0);
      Target_Ext_OID       : constant SNMP_Defines.OID_Max_Array := (1, 3, 6, 1, 2, 1, 31,  1,  1,             others => 0);
      Target_ADS_OID       : constant SNMP_Defines.OID_Max_Array := (1, 3, 6, 1, 2, 1, 10, 94,  1, 1,          others => 0);
      SNMP_Engine_Time     : constant SNMP_Defines.OID_Max_Array := (1, 3, 6, 1, 6, 3, 10,  2,  1, 3,          others => 0);
--      Sys_Uptime_OID       : constant SNMP_Defines.OID_Max_Array := (1, 3, 6, 1, 2, 1,  1,  3,                 others => 0);
--      Last_Update_OID      : constant SNMP_Defines.OID_Max_Array := (1, 3, 6, 1, 4, 1,  9,  9, 43, 1, 1, 1, 0, others => 0);
      Target_OID           :          SNMP_Defines.OID_Max_Array;
      Target_Int_Len       : constant Interfaces.C.size_t :=  9;
      Target_Ext_Len       : constant Interfaces.C.size_t :=  9;
      Target_ADS_Len       : constant Interfaces.C.size_t := 10;
      Target_Len           :          Interfaces.C.size_t;
      Current_OID          :          SNMP_Defines.OID_Max_Array;
      Current_Len          :          Interfaces.C.size_t;
      Current_PDU          : access   SNMP_Defines.SNMP_PDU;
      Response_PDU         : aliased  SNMP_Defines.SNMP_PDU;
      Res                  :          Interfaces.C.int := 0;
      Running              :          Boolean := True;
      Do_Exit              :          Boolean := False;
      Short_Duration       : constant Duration := 0.1;
      Long_Duration        : constant Duration := 15.0 * 60.0;
      Time_Target_Short    : Ada.Calendar.Time;
      Time_Target_SNMP     : Ada.Calendar.Time;
      Tmp_Clock            : Ada.Calendar.Time;
      Year_of_Time         : Ada.Calendar.Year_Number;
      Month_of_Time        : Ada.Calendar.Month_Number;
      Day_of_Time          : Ada.Calendar.Day_Number;
      Hour_of_Time         : GNAT.Calendar.Hour_Number;
      Minute_of_Time       : GNAT.Calendar.Minute_Number;
      Seconds_of_Time      : GNAT.Calendar.Second_Number;
      Sub_Seconds_of_Time  : GNAT.Calendar.Second_Duration;
      Host_Boot_Time       : Ada.Calendar.Time;
      Host_Duration        : Duration;
--      Sys_Uptime           : Interfaces.C.long;
--      Last_Change_Time     : Interfaces.C.long;
      TS_Start             : TS_Record;
      TS_End               : TS_Record;
      Tmp_Long             : Interfaces.C.long;
      Valid_Count          : Integer := 1;
      No_named_exception   : exception;
      All_OIDs             : constant array (1 .. 3) of OID_List_Record := (
                                                                            (Target_Int_OID, Target_Int_Len, "Int"),
                                                                            (Target_Ext_OID, Target_Ext_Len, "Ext"),
                                                                            (Target_ADS_OID, Target_ADS_Len, "ADS")
                                                                           );
   begin
      accept Start;
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Initialising SNMP");
      SNMP_Defines.Init_SNMP (SNMP_Agent_Name);
      SNMP_Defines.SNMP_Sess_Init (Session_Start'Address);
      Session_Start.PeerName             := Interfaces.C.Strings.New_String ("router.nevis");
      Session_Start.Version              := SNMP_Defines.SNMP_VERSION_3;
      Session_Start.SecurityName         := Interfaces.C.Strings.New_String ("b");
      Session_Start.SecurityNameLen      := Interfaces.C.Strings.Strlen (Session_Start.SecurityName);
      Session_Start.SecurityLevel        := SNMP_Defines.SNMP_SEC_LEVEL_AUTHNOPRIV;
      Session_Start.SecurityAuthProto    := SNMP_Defines.usmHMACSHA1AuthProtocol'Address;
      Session_Start.SecurityAuthProtoLen := SNMP_Defines.usmHMACSHA1AuthProtocol'Length;
      Session_Start.SecurityAuthKeyLen   := SNMP_Defines.USM_AUTH_KU_LEN;
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Generating Ku");

      if SNMP_Defines.Generate_Ku (Session_Start.SecurityAuthProto, Session_Start.SecurityAuthProtoLen, Password, Interfaces.C.Strings.Strlen (Password), Session_Start.SecurityAuthKey, Session_Start.SecurityAuthKeyLen'Access) /= 0 then
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Why is Res /= 0? - " & GNAT.Source_Info.Source_Location);
      end if;

      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Opening SNMP session");
      Session_Opaque := SNMP_Defines.SNMP_Open (Session_Start'Access);

      snmp_valid :
      while Session_Opaque = null loop
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "SNMP_Open returned null, Valid_Count: " & Valid_Count'Image & " - " & GNAT.Source_Info.Source_Location);
         if Valid_Count > 120 then
            raise No_named_exception;
         end if;
         delay 10.0;
         Valid_Count := Valid_Count + 1;
         Session_Opaque := SNMP_Defines.SNMP_Open (Session_Start'Access);
      end loop snmp_valid;

--      PDU_Details := SNMP_Defines.SNMP_PDU_Create (SNMP_Defines.SNMP_MSG_GET);
--      PDU_Details.all.ErrStat := 0;
--      PDU_Details.all.ErrIndex := 1;

      if SNMP_Defines.NetSNMP_Get_DS_Boolean (SNMP_Defines.NETSNMP_DS_APPLICATION_ID, SNMP_Defines.NETSNMP_DS_WALK_DONT_CHECK_LEXICOGRAPHIC) /= 0 then
         null;
      end if;

--      PDU_Details := SNMP_Defines.SNMP_PDU_Create (SNMP_Defines.SNMP_MSG_GET);
--      PDU_Details.all.ErrStat := 0;
--      PDU_Details.all.ErrIndex := 1;
--      SNMP_Defines.SNMP_Add_Null_Var (PDU_Details, SNMP_Engine_Time'Address,  11);
--      SNMP_Defines.SNMP_Synch_Response (Session_Opaque, PDU_Details, Response_PDU, Res);
--
--      if Res = SNMP_Defines.STAT_SUCCESS then
--         Var_List := Response_PDU.Variables;
--         Tmp_Long := Var_List.all.Val.integer.all;
--         Host_Duration := Duration (Tmp_Long);
--         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, " Host_Duration: " & Host_Duration'Image & ", Tmp_Long: " & Tmp_Long'Image);
--      end if;

--      SNMP_Defines.SNMP_Add_Null_Var (PDU_Details, Sys_Uptime_OID'Address,  Sys_Uptime_OID'Length);
--      SNMP_Defines.SNMP_Add_Null_Var (PDU_Details, Last_Update_OID'Address, Last_Update_OID'Length);
--      SNMP_Defines.SNMP_Synch_Response (Session_Opaque, PDU_Details, Response_PDU, Res);

--      if Res = SNMP_Defines.STAT_SUCCESS then
--         Var_List := Response_PDU.Variables;
--         Host_Up_Time := Ada.Calendar.Clock - Duration (Var_List.all.Val.integer.all);  --  Current time - host up time in seconds = Time of last boot

--         if DB_Routines.Update_Host_Up_Time (1, Host_Up_Time) /= Local_Defs.TTrue then
--            Ada.Text_IO.Put_Line ("Why failed?");
--         end if;
--         Ada.Text_IO.Put_Line ("Uptime...: " & Sys_Uptime'Image);
--         Var_List := Var_List.all.Next_Variable;
--         Last_Change_Time := Var_List.all.Val.integer.all;
--         Ada.Text_IO.Put_Line ("Last change...: " & Last_Change_Time'Image);
--         Do_Exit := True;
--      end if;

      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Time calculation");
      Time_Target_SNMP  := Ada.Calendar.Clock + Short_Duration;
      GNAT.Calendar.Split (Time_Target_SNMP, Year_of_Time, Month_of_Time, Day_of_Time, Hour_of_Time, Minute_of_Time, Seconds_of_Time, Sub_Seconds_of_Time);
      Time_Target_Short := GNAT.Calendar.Time_Of (Year_of_Time, Month_of_Time, Day_of_Time, Hour_of_Time, Minute_of_Time, Seconds_of_Time, 0.0) + 1.0 + Short_Duration;
      Time_Target_SNMP  := GNAT.Calendar.Time_Of (Year_of_Time, Month_of_Time, Day_of_Time, Hour_of_Time, 0, 0, 0.0) + Long_Duration;

      if Time_Target_SNMP < Time_Target_Short then
         Time_Target_SNMP := Time_Target_SNMP + Long_Duration;
      end if;
      if Time_Target_SNMP < Time_Target_Short then
         Time_Target_SNMP := Time_Target_SNMP + Long_Duration;
      end if;
      if Time_Target_SNMP < Time_Target_Short then
         Time_Target_SNMP := Time_Target_SNMP + Long_Duration;
      end if;

      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Time_Target_Short: " & Ada.Calendar.Formatting.Image (Time_Target_Short) & ", Time_Target_SNMP: " & Ada.Calendar.Formatting.Image (Time_Target_SNMP));

      busy_loop : loop
         timing_loop :
         while Time_Target_Short < Time_Target_SNMP loop
            select
               accept Kill_Me  do
                  Do_Exit := True;
               end Kill_Me;
            else
               if Do_Exit then
                  exit timing_loop;
               end if;

               Time_Target_Short := Time_Target_Short + Short_Duration;
               delay until (Time_Target_Short);
            end select;
         end loop timing_loop;

         if Do_Exit then
            exit busy_loop;
         end if;

         Clock_Get_Time (CLOCK_REALTIME, TS_Start'Address);
         Tmp_Clock := Ada.Calendar.Clock;

         parse_all_oids :
         for Z of All_OIDs loop
            Target_OID := Z.OID;
            Target_Len := Z.Len;
            Current_OID := Target_OID;
            Current_Len := Target_Len;
            Running := True;

            major :
            while Running loop
               Current_PDU := SNMP_Defines.SNMP_PDU_Create (SNMP_Defines.SNMP_MSG_GETBULK);
               Current_PDU.all.ErrStat  :=  0; --  non_repeaters
               Current_PDU.all.ErrIndex := 10; --  max_repetitions
               SNMP_Defines.SNMP_Add_Null_Var (Current_PDU, Current_OID'Address,  Current_Len);
               SNMP_Defines.SNMP_Synch_Response (Session_Opaque, Current_PDU, Response_PDU, Res);

               if Res = SNMP_Defines.STAT_SUCCESS then
                  if Response_PDU.ErrStat = SNMP_Defines.SNMP_ERR_NOERROR then
                     Var_List := Response_PDU.Variables;

                     Var_List_Loop :
                     while Var_List /= null loop
                        if Var_List.all.Name_Length < Target_Len or else SNMP_Defines.SNMP_OID_Compare (Target_OID'Address, Target_Len, Var_List.all.Name.all'Address, Target_Len) < 0 then
                           Running := False;
                           exit Var_List_Loop;
                        else
                           if Z.Desc = "Int" then
                              Populate_Int;
                           elsif Z.Desc = "Ext" then
                              Populate_ExInt;
                           elsif Z.Desc = "ADS" then
                              Populate_ADSL;
                           end if;

                           if Var_List.all.C_Type /= SNMP_Defines.SNMP_NOSUCHOBJECT and then Var_List.all.C_Type /= SNMP_Defines.SNMP_NOSUCHINSTANCE and then  Var_List.all.C_Type /= SNMP_Defines.SNMP_ENDOFMIBVIEW then
                              if Var_List.all.Next_Variable = null then -- Take reading across packet bounderies
                                 Current_OID := Var_List.all.Name.all;
                                 Current_Len := Var_List.all.Name_Length;
                              end if;
                           else
                              Running := False;
                              exit Var_List_Loop;
                           end if;
                        end if;
                        Var_List := Var_List.all.Next_Variable;
                     end loop Var_List_Loop;

                     if not Running then
                        exit major;
                     end if;
                  else
                     Running := False;
                     if Response_PDU.ErrStat = SNMP_ERR_NOSUCHNAME then
                        GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "End of mib, " & GNAT.Source_Info.Source_Location);
                     else
                        GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Error in SNMP packet, " & GNAT.Source_Info.Source_Location);
                     end if;
                  end if;
               elsif Res = STAT_TIMEOUT then
                  GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Timeout: No Response from " & Interfaces.C.Strings.Value (Session_Opaque.all.PeerName) & ", " & GNAT.Source_Info.Source_Location);
                  Running := False;
               else
                  SNMP_Defines.SNMP_Session_PError (Interfaces.C.Strings.New_String ("intsage"), Session_Opaque);
                  Running := False;
               end if;
            end loop major;
         end loop parse_all_oids;

         for C in Data_Map_ADSL.Iterate loop
            List_Handlers.Secure.Push (SQL_Helpers.ADSL_SQL (TS_Start.TV_Sec, Data_Map_ADSL.Reference (C).Element.all),
                                       TS_Start.TV_Sec,
                                       Data_Map_ADSL.Reference (C).Element.all.ifIndex);
         end loop;

         for C in Data_Map_Int.Iterate loop
            List_Handlers.Secure.Push (SQL_Helpers.Int_SQL (TS_Start.TV_Sec, Data_Map_Int.Reference (C).Element.all),
                                       TS_Start.TV_Sec,
                                       Data_Map_Int.Reference (C).Element.all.ifIndex,
                                       Data_Map_Int.Reference (C).Element.all.ifDesc);
         end loop;

         PDU_Details := SNMP_Defines.SNMP_PDU_Create (SNMP_Defines.SNMP_MSG_GET);
         PDU_Details.all.ErrStat := 0;
         PDU_Details.all.ErrIndex := 1;
         SNMP_Defines.SNMP_Add_Null_Var (PDU_Details, SNMP_Engine_Time'Address,  11);
         SNMP_Defines.SNMP_Synch_Response (Session_Opaque, PDU_Details, Response_PDU, Res);

         if Res = SNMP_Defines.STAT_SUCCESS then
            Var_List := Response_PDU.Variables;
            Tmp_Long := Var_List.all.Val.integer.all;
            Host_Duration := Duration (Tmp_Long);
            Host_Boot_Time := Tmp_Clock - Host_Duration;  --  Current time - host up time in seconds = Time of last boot
--            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Tmp_Clock: " & Ada.Calendar.Conversions.To_Unix_Time (Tmp_Clock)'Image & ", Host_Duration: " & Host_Duration'Image & ", Tmp_Long: " & Tmp_Long'Image);

            List_Handlers.Push (1, Ada.Calendar.Conversions.To_Unix_Time (Host_Boot_Time));
         end if;

         Clock_Get_Time (CLOCK_REALTIME, TS_End'Address);
         Ada.Integer_Text_IO.Default_Width := 10;
         Ada.Integer_Text_IO.Put (Tmp_Start_Sec,  Integer (TS_Start.TV_Sec));
         Ada.Integer_Text_IO.Put (Tmp_End_Sec,    Integer (TS_End.  TV_Sec));
         Ada.Integer_Text_IO.Default_Width := 9;
         Ada.Integer_Text_IO.Put (Tmp_Start_NSec, Integer (TS_Start.TV_NSec));
         Ada.Integer_Text_IO.Put (Tmp_End_NSec,   Integer (TS_End.  TV_NSec));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical,
                                        "Start seconds: " & Tmp_Start_Sec & ", nanoseconds: " & Tmp_Start_NSec &
                                        ", end seconds: " & Tmp_End_Sec   & ", nanoseconds: " & Tmp_End_NSec);
         Data_Map_Int.Clear;
         Data_Map_ADSL.Clear;
         Time_Target_SNMP  := Time_Target_SNMP + Long_Duration;
         Time_Target_Short := Time_Target_Short + Short_Duration;
      end loop busy_loop;

   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exception raised, "  &
                                          GNAT.Source_Info.Source_Location & ", " & Ada.Exceptions.Exception_Information (E) & ", " & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         raise;
   end SNMP_Code_Loop;
end SNMP_Code;
