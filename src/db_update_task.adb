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
with Interfaces.C;
with GNAT.Source_Info;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Traces.Syslog;
with CTRL_C_Handler;
with DB_Routines;
with List_Handlers;
with Local_Defs; use Local_Defs;

package body DB_Update_Task is

   task body Update is
      Do_Exit      : Boolean := False;
      Res          : Boolean;
      Res_Conn     : Local_Defs.Trilean;
      ADSL_Str     : Ada.Strings.Unbounded.Unbounded_String;
      Int_Rec      : List_Handlers.Int_Record_Type;
      T            : Interfaces.C.long;
      Trace        : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
      Length       : Natural;
   begin
      select
         accept Start;
      end select;

      if DB_Routines.DB_Connect /= Local_Defs.TTrue then
         Do_Exit := True;
      end if;

      busy : loop
         select
            accept Kill_Me do
               Do_Exit := True;
            end Kill_Me;
         else
            if List_Handlers.Secure.Is_ADSL_Data_Avail then
               List_Handlers.Secure.Peek (ADSL_Str, T);
               Res := DB_Routines.Insert_ADSL_data (ADSL_Str);

               if Res = False then
                  DB_Routines.DB_Disconnect;
                  Res_Conn := DB_Routines.DB_Connect;
                  if Res_Conn = TFalse or else Res_Conn = TBroken then
                     CTRL_C_Handler.Monitor_CTRL_C_Called;
                     exit busy;
                  end if;
               end if;

               List_Handlers.Secure.Remove_ADSL (ADSL_Str, T);
            end if;

            if List_Handlers.Secure.Is_Int_Data_Avail then
               List_Handlers.Secure.Peek (Int_Rec, T);
               Res_Conn := DB_Routines.Is_Int_Exist (1, Int_Rec.ifDesc, Int_Rec.ifIndex);

               if Res_Conn = Local_Defs.TBroken then
                  DB_Routines.DB_Disconnect;
                  Res_Conn := DB_Routines.DB_Connect;
                  if Res_Conn = TFalse or else Res_Conn = TBroken then
                     CTRL_C_Handler.Monitor_CTRL_C_Called;
                     exit busy;
                  end if;
               elsif Res_Conn = Local_Defs.TTrue then
                  Res_Conn := DB_Routines.Update_Int (1, Int_Rec.ifDesc, Int_Rec.ifIndex, T);
                  if Res_Conn = Local_Defs.TBroken then
                     DB_Routines.DB_Disconnect;
                     Res_Conn := DB_Routines.DB_Connect;
                     if Res_Conn = TFalse or else Res_Conn = TBroken then
                        CTRL_C_Handler.Monitor_CTRL_C_Called;
                        exit busy;
                     end if;
                  end if;
               else
                  Res_Conn := DB_Routines.Insert_Int (1, Int_Rec.ifDesc, Int_Rec.ifName, Int_Rec.ifIndex, T);
                  if Res_Conn = Local_Defs.TBroken then
                     DB_Routines.DB_Disconnect;
                     Res_Conn := DB_Routines.DB_Connect;
                     if Res_Conn = TFalse or else Res_Conn = TBroken then
                        CTRL_C_Handler.Monitor_CTRL_C_Called;
                        exit busy;
                     end if;
                  end if;
               end if;

               Res := DB_Routines.Insert_Int_Data (Int_Rec.SQL);

               if Res = False then
                  DB_Routines.DB_Disconnect;
                  Res_Conn := DB_Routines.DB_Connect;
                  if Res_Conn = TFalse or else Res_Conn = TBroken then
                     CTRL_C_Handler.Monitor_CTRL_C_Called;
                     exit busy;
                  end if;
               end if;

               List_Handlers.Secure.Remove_Int (Int_Rec.SQL, T);
            end if;

            while List_Handlers.Is_Host_Data_Avail loop
               declare
                  HU : List_Handlers.Host_Uptime_Type;
               begin
                  HU := List_Handlers.Pop;
                  Res_Conn := DB_Routines.Update_Host_Up_Time (HU.Host, HU.Uptime);

                  if Res_Conn /= Local_Defs.TTrue then
                     GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location);
                  end if;
               exception
                  when E : others =>
                     GNAT.Traceback.Call_Chain (Trace, Length);
                     GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                                      GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
                     GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
               end;
            end loop;
         end select;

         if Do_Exit then
            exit busy;
         end if;

         delay 0.6;
      end loop busy;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Update;
end DB_Update_Task;
