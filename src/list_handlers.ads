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

with Ada.Containers.Ordered_Maps; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Interfaces.C; use Interfaces.C;

package List_Handlers is
   type Int_Record_Type is record
      SQL     : Ada.Strings.Unbounded.Unbounded_String;
      ifDesc  : Ada.Strings.Unbounded.Unbounded_String;
      ifName  : Ada.Strings.Unbounded.Unbounded_String;
      ifIndex : Interfaces.C.long;
   end record;

   type Host_Uptime_Type is record
      Host : Integer;
      Uptime : Interfaces.C.long;
   end record;

   package Int_SQL_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Interfaces.C.long,
      Element_Type => Int_Record_Type);
   subtype Int_SQL_Cursor is Int_SQL_Map.Cursor;

   package ADSL_SQL_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Interfaces.C.long,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String);
   subtype ADSL_SQL_Cursor is ADSL_SQL_Map.Cursor;

   package Host_SQL_Map_Interface is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type     => Host_Uptime_Type);
   package Host_SQL_Map           is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Host_SQL_Map_Interface);

   function  Is_Host_Data_Avail return Boolean;
   function  Pop               return List_Handlers.Host_Uptime_Type;
   procedure Push         (Host_ID :     Integer;                                Host_Uptime :     Interfaces.C.long);

   protected Secure is
      function Is_ADSL_Data_Avail return Boolean;
      function Is_Int_Data_Avail  return Boolean;
      procedure Peek         (S       : out Ada.Strings.Unbounded.Unbounded_String; T           : out Interfaces.C.long);
      procedure Peek         (R       : out Int_Record_Type;                        T           : out Interfaces.C.long);
      procedure Push         (S       :     Ada.Strings.Unbounded.Unbounded_String; T           :     Interfaces.C.long; I : Interfaces.C.long);
      procedure Push         (S       :     Ada.Strings.Unbounded.Unbounded_String; T           :     Interfaces.C.long; I : Interfaces.C.long; D : Ada.Strings.Unbounded.Unbounded_String);
      procedure Remove_ADSL  (S       :     Ada.Strings.Unbounded.Unbounded_String; T           :     Interfaces.C.long);
      procedure Remove_Int   (S       :     Ada.Strings.Unbounded.Unbounded_String; T           :     Interfaces.C.long);
   private
      Int_SQL  : Int_SQL_Map.Map;
      ADSL_SQL : ADSL_SQL_Map.Map;
   end Secure;

private
   Host_SQL : Host_SQL_Map.Queue;
end List_Handlers;
