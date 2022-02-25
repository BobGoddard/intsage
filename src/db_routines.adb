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

with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;
with AdaBase; use AdaBase;
with AdaBase.Results.Sets;
with AdaBase.Statement;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Traces.Syslog;
with CTRL_C_Handler;

package body DB_Routines is
   function DB_Connect return Local_Defs.Trilean is
      delay_time_max   : constant Duration := 65536.0;
      delay_time       : Duration := 1.0;
      delay_time_count : Duration := 0.0;
      target_time      : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      DB_Disconnect;
      delay_loop :
      loop
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Contacting DB, delaying until " & Ada.Calendar.Formatting.Image (target_time) & " - " & GNAT.Source_Info.Source_Location);
         delay until target_time;

         if CTRL_C_Handler.Monitor_CTRL_C_Is_Called = True or else (DB_Connect_Private = Local_Defs.TFalse and then delay_time_count >= delay_time_max) then
            return Local_Defs.TFalse;
         end if;

         if DB_Connect_Private = Local_Defs.TTrue then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Connected - " & GNAT.Source_Info.Source_Location);
            return Local_Defs.TTrue;
         end if;

         if delay_time_count = delay_time then
            delay_time := delay_time * 2.0;
            delay_time_count := 0.0;
         else
            delay_time := delay_time + 1.0;
         end if;

         target_time := target_time + delay_time;
         delay_time_count := delay_time_count + 1.0;
      end loop delay_loop;

   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Local_Defs.TBroken;
   end DB_Connect;

   function DB_Connect_Private return Local_Defs.Trilean is
   begin
      DR.basic_connect (database => Ada.Strings.Unbounded.To_String (Database),
                        username => Ada.Strings.Unbounded.To_String (DB_User),
                        password => Ada.Strings.Unbounded.To_String (DB_Pass),
                        hostname => Ada.Strings.Unbounded.To_String (DB_Host),
                        port     => DB_Port);

      return Local_Defs.TTrue;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Local_Defs.TBroken;
   end DB_Connect_Private;

   procedure DB_Disconnect is
   begin
      DR.disconnect;
   end DB_Disconnect;

   function Insert_ADSL_data (A : Ada.Strings.Unbounded.Unbounded_String) return Boolean is
   begin
      declare
         STMT : Stmt_Type_Local := DR.prepare (Ada.Strings.Unbounded.To_String (A));
      begin
         if STMT.execute then
            DR.commit;
         else
            DR.rollback;
            return False;
         end if;
      end;

      return True;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet - A: " & Ada.Strings.Unbounded.To_String (A) & ", " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return False;
   end Insert_ADSL_data;

   function Insert_Int      (H : Integer; ifDesc, ifName : Ada.Strings.Unbounded.Unbounded_String; I : Interfaces.C.long; TS : Interfaces.C.long) return Local_Defs.Trilean is
      NS   : constant String := "INSERT INTO interface (hostid, interface, shortname, ifIndex, ts, last_seen) values (" &
               H'Image & "," &
               "'" & Ada.Strings.Unbounded.To_String (ifDesc) & "'," &
               "'" & Ada.Strings.Unbounded.To_String (ifName) & "'," &
               I'Image & "," &
               Integer (TS / 1000)'Image & "," &
               Integer (TS / 1000)'Image & ")";
      STMT : Stmt_Type_Local := DR.prepare (NS);
   begin
      if STMT.execute then
         DR.commit;
         return Local_Defs.TTrue;
      else
         DR.rollback;
      end if;

      return Local_Defs.TBroken;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet - NS: " & NS & ", " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Local_Defs.TFalse;
   end Insert_Int;

   function Insert_Int_Data (I : Ada.Strings.Unbounded.Unbounded_String) return Boolean is
   begin
      declare
         STMT : Stmt_Type_Local := DR.prepare (Ada.Strings.Unbounded.To_String (I));
      begin
         if STMT.execute then
            DR.commit;
         else
            DR.rollback;
            return False;
         end if;
      end;

      return True;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet - I: " & Ada.Strings.Unbounded.To_String (I) & ", " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return False;
   end Insert_Int_Data;

   function Is_Int_Exist     (H : Integer; ifDesc : Ada.Strings.Unbounded.Unbounded_String; I : Interfaces.C.long) return Local_Defs.Trilean is
      NS   : constant String := "SELECT hostid FROM interface WHERE hostid=" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Left) & " AND interface='" & Ada.Strings.Unbounded.To_String (ifDesc) & "' AND ifIndex=" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Left) & " LIMIT 1";
      STMT : Stmt_Type_Local := DR.prepare (NS);
   begin
      if STMT.execute then
         if STMT.rows_returned = 0 then
            return Local_Defs.TFalse;
         else
            return Local_Defs.TTrue;
         end if;
      end if;

      return Local_Defs.TBroken;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          "NS: " & NS & ", " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Local_Defs.TBroken;
   end Is_Int_Exist;

   procedure Set_Account_Details (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306) is
   begin
      Database := DB;
      DB_Host  := Host;
      DB_User  := User;
      DB_Pass  := Pass;
      DB_Port  := Port;
   end Set_Account_Details;

   function  Update_Host_Up_Time (Host_ID : Integer; Host_Boot_Time : Interfaces.C.long) return Local_Defs.Trilean is
      NS_Select      : constant String := "SELECT boot_time FROM host_boot_time WHERE hostid=" & Ada.Strings.Fixed.Trim (Host_ID'Image, Ada.Strings.Left) & " order by hostid,boot_time desc LIMIT 1";
      NS_Insert      : constant String := "INSERT INTO host_boot_time VALUES (" & Ada.Strings.Fixed.Trim (Host_ID'Image, Ada.Strings.Left) & "," & Ada.Strings.Fixed.Trim (Host_Boot_Time'Image, Ada.Strings.Left) & ")";
      STMT_Select    : Stmt_Type_Local := DR.prepare (NS_Select);
      STMT_Insert    : Stmt_Type_Local := DR.prepare (NS_Insert);
      Row            : AdaBase.Results.Sets.Datarow;
      Last_Boot_Time : Interfaces.C.long;
   begin
      if STMT_Select.execute then
         if STMT_Select.rows_returned = 0 then  -- If host does no already exist in table, then just insert
            if not STMT_Insert.execute then
               return Local_Defs.TFalse;
            end if;
         else
            Row := STMT_Select.fetch_next;
            Last_Boot_Time := Interfaces.C.long (Row.column  (1).as_nbyte8);

            if Host_Boot_Time - Last_Boot_Time > 60 * 15 then
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Host_Boot_Time: " & Host_Boot_Time'Image & " - Last_Boot_Time: " & Last_Boot_Time'Image);
               if not STMT_Insert.execute then
                  return Local_Defs.TFalse;
               end if;
            end if;
         end if;

         return Local_Defs.TTrue;
      end if;

      return Local_Defs.TBroken;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet: host id: " & Host_ID'Image & ", host boot time: " & Last_Boot_Time'Image &
                                          " NS_Select: " & NS_Select &
                                          " NS_Insert: " & NS_Insert & ", " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Local_Defs.TBroken;
   end Update_Host_Up_Time;

   function Update_Int       (H : Integer; ifDesc : Ada.Strings.Unbounded.Unbounded_String; I : Interfaces.C.long; LS : Interfaces.C.long) return Local_Defs.Trilean is
      NS   : constant String := "UPDATE interface SET last_seen=" & Integer (LS / 1000)'Image & " WHERE hostid=" & H'Image & " AND interface='" & Ada.Strings.Unbounded.To_String (ifDesc) & "' AND ifIndex=" & I'Image;
      STMT : Stmt_Type_Local := DR.prepare (NS);
   begin
      if STMT.execute then
         DR.commit;
         return Local_Defs.TTrue;
      else
         DR.rollback;
      end if;

      return Local_Defs.TBroken;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet - NS: " &
                                          NS & ", " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Local_Defs.TBroken;
   end Update_Int;

end DB_Routines;
