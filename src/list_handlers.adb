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

with Ada.Exceptions;
with GNAT.Source_Info;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Traces.Syslog;

package body List_Handlers is
   Trace        : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length       : Natural;
   function Is_Host_Data_Avail return Boolean is
   begin
      if Host_SQL.Current_Use = 0 then
         return False;
      end if;

      return True;
   end Is_Host_Data_Avail;

   function Pop return Host_Uptime_Type is
      HU : Host_Uptime_Type;
   begin
      Host_SQL.Dequeue (HU);
      return HU;
   end Pop;

   procedure Push (Host_ID : Integer; Host_Uptime : Interfaces.C.long) is
      HU : Host_Uptime_Type;
   begin
      HU.Host   := Host_ID;
      HU.Uptime := Host_Uptime;
      Host_SQL.Enqueue (HU);
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Push;

   protected body Secure is
      function Is_ADSL_Data_Avail return Boolean is
      begin
         if ADSL_SQL.Is_Empty then
            return False;
         end if;

         return True;
      end Is_ADSL_Data_Avail;

      function Is_Int_Data_Avail return Boolean is
      begin
         if Int_SQL.Is_Empty then
            return False;
         end if;

         return True;
      end Is_Int_Data_Avail;

      procedure Peek   (S : out Ada.Strings.Unbounded.Unbounded_String; T : out Interfaces.C.long) is
         C : ADSL_SQL_Cursor;
      begin
         C := ADSL_SQL.First;
         T := ADSL_SQL_Map.Key (C);
         S := ADSL_SQL_Map.Element (C);
      exception
         when E : others =>
            GNAT.Traceback.Call_Chain (Trace, Length);
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                             GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
      end Peek;

      procedure Peek   (R : out Int_Record_Type; T : out Interfaces.C.long) is
         C : Int_SQL_Cursor;
      begin
         C := Int_SQL.First;
         T := Int_SQL_Map.Key (C);
         R := Int_SQL_Map.Element (C);
      exception
         when E : others =>
            GNAT.Traceback.Call_Chain (Trace, Length);
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                             GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
      end Peek;

      procedure Push   (S : Ada.Strings.Unbounded.Unbounded_String; T : Interfaces.C.long; I : Interfaces.C.long) is
      begin
         ADSL_SQL.Insert (T * 1000 + I, S);
      exception
         when E : others =>
            GNAT.Traceback.Call_Chain (Trace, Length);
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                             GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
      end Push;

      procedure Push   (S : Ada.Strings.Unbounded.Unbounded_String; T : Interfaces.C.long; I : Interfaces.C.long; D : Ada.Strings.Unbounded.Unbounded_String) is
         R : List_Handlers.Int_Record_Type;
      begin
         R.SQL     := S;
         R.ifIndex := I;
         R.ifDesc  := D;
         Int_SQL.Insert (T * 1000 + I, R);
      exception
         when E : others =>
            GNAT.Traceback.Call_Chain (Trace, Length);
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                             GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
      end Push;

      procedure Remove_ADSL (S : Ada.Strings.Unbounded.Unbounded_String; T : Interfaces.C.long) is
         C : ADSL_SQL_Cursor;
      begin
         C := ADSL_SQL.Find (T);

         if ADSL_SQL_Map.Element (C) = S then
            ADSL_SQL.Delete (C);
         end if;
      exception
         when E : others =>
            GNAT.Traceback.Call_Chain (Trace, Length);
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                             GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
      end Remove_ADSL;

      procedure Remove_Int (S : Ada.Strings.Unbounded.Unbounded_String; T : Interfaces.C.long) is
         C : Int_SQL_Cursor;
      begin
         C := Int_SQL.Find (T);

         if Int_SQL_Map.Element (C).SQL = S then
            Int_SQL.Delete (C);
         end if;
      exception
         when E : others =>
            GNAT.Traceback.Call_Chain (Trace, Length);
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                             GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
      end Remove_Int;

   end Secure;
end List_Handlers;
