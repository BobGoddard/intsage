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
with Ada.Strings.Unbounded;
with GNAT.Ctrl_C;
--  with GNAT.Sockets; use GNAT.Sockets;
with GNAT.Source_Info;
with GNATCOLL.Traces.Syslog;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;
with CTRL_C_Handler;
with DB_Update_Task;
with SNMP_Code;
with Config_Handler;

procedure Intsage is
   SNMP_Task    : SNMP_Code.SNMP_Code_Loop;
   DB_Task      : DB_Update_Task.Update;
   Trace        : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length       : Natural;
   XML_Settings : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("/etc/intsage.xml");
--   Tmp_Count  : Integer := 0;
--   Host_Found : Boolean := False;
--   Tmp_Addr   : GNAT.Sockets.Sock_Addr_Type;
--   pragma Unreferenced (Tmp_Addr);
begin
   GNATCOLL.Traces.Syslog.Register_Syslog_Stream;
   GNATCOLL.Traces.Syslog.Openlog ("intsage", GNATCOLL.Traces.Syslog.None, GNATCOLL.Traces.Syslog.Daemon);
   GNAT.Ctrl_C.Install_Handler (CTRL_C_Handler.Monitor_CTRL_C_Called'Access);

--   Get_Host_Address :
--   loop
--      begin
--         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Resolving router address");
--         Tmp_Addr.Addr := GNAT.Sockets.Addresses (GNAT.Sockets. .Inet_Addr ("81.187.35.206"), 1);
----         Tmp_Addr.Addr := GNAT.Sockets.Addresses (Get_Host_By_Name ("router"), 1);
--         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Router address resolved");
--         Host_Found := True;
--         exit Get_Host_Address;
--      exception
--         when E : Host_Error =>
--            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Unable to resolve hostname of router, "  &
--                                             GNAT.Source_Info.Source_Location & ", " & Ada.Exceptions.Exception_Information (E));
--            delay 5.0;
--            Tmp_Count := Tmp_Count + 1;
--            if Tmp_Count = 60 * 15 then
--               exit Get_Host_Address;
--            end if;
--      end;
--   end loop Get_Host_Address;

--   if not Host_Found then
--      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Unable to resolve hostname of router, exiting..."  & GNAT.Source_Info.Source_Location);
--      return;
--   end if;

   Config_Handler.Set_Config_Name (XML_Settings);
   Config_Handler.Load_Config;
   SNMP_Task.Start;
   DB_Task.Start;

   sleep_loop :
   while not CTRL_C_Handler.Monitor_CTRL_C_Is_Called loop
      if SNMP_Task'Terminated or else DB_Task'Terminated then
         exit sleep_loop;
      end if;

      delay 0.09;
   end loop sleep_loop;

   if not SNMP_Task'Terminated then
      SNMP_Task.Kill_Me;
   end if;

   if not DB_Task'Terminated then
      DB_Task.Kill_Me;
   end if;
exception
   when E : others =>
      GNAT.Traceback.Call_Chain (Trace, Length);
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                       GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
end Intsage;
