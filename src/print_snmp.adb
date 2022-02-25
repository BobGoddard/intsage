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

with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body Print_SNMP is
   Trace        : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length       : Natural;
   procedure Print (Data_Map_Int : in out IntSage_Map.Int_Sage_Map.Map; Data_Map_ADSL : in out IntSage_Map.ADSL_Sage_Map.Map) is
   begin
      --  SNMP_Defines.Print_SNMP_Variable (Var_List.all.name.all'Address, Var_List.all.name_length, Var_List.all);
      Ada.Text_IO.Put_Line ("ifIndex"                & Ada.Characters.Latin_1.HT &
                              "ifDesc"               & Ada.Characters.Latin_1.HT &
                              "ifAdminStatus"        & Ada.Characters.Latin_1.HT &
                              "ifOperStatus"         & Ada.Characters.Latin_1.HT &
                              "ifLastChange"         & Ada.Characters.Latin_1.HT &
                              "ifInOctets"           & Ada.Characters.Latin_1.HT &
                              "ifInDiscards"         & Ada.Characters.Latin_1.HT &
                              "ifInErrors"           & Ada.Characters.Latin_1.HT &
                              "ifInUnknownProtos"    & Ada.Characters.Latin_1.HT &
                              "ifOutOctets"          & Ada.Characters.Latin_1.HT &
                              "ifOutDiscards"        & Ada.Characters.Latin_1.HT &
                              "ifOutErrors"          & Ada.Characters.Latin_1.HT &
                              "ifInMulticastPkts"    & Ada.Characters.Latin_1.HT &
                              "ifInBroadcastPkts"    & Ada.Characters.Latin_1.HT &
                              "ifOutMulticastPkts"   & Ada.Characters.Latin_1.HT &
                              "ifOutBroadcastPkts"   & Ada.Characters.Latin_1.HT &
                              "ifHCInOctets"         & Ada.Characters.Latin_1.HT &
                              "ifHCInUcastPkts"      & Ada.Characters.Latin_1.HT &
                              "ifHCInMulticastPkts"  & Ada.Characters.Latin_1.HT &
                              "ifHCInBroadcastPkts"  & Ada.Characters.Latin_1.HT &
                              "ifHCOutOctets"        & Ada.Characters.Latin_1.HT &
                              "ifHCOutUcastPkts"     & Ada.Characters.Latin_1.HT &
                              "ifHCOutMulticastPkts" & Ada.Characters.Latin_1.HT &
                              "ifHCOutBroadcastPkts" & Ada.Characters.Latin_1.HT &
                              "ifCounterDiscontinuityTime"
                           );

      for C in Data_Map_Int.Iterate loop
         Ada.Text_IO.Put (Data_Map_Int.Reference (C).Element.all.ifIndex'Image & Ada.Characters.Latin_1.HT);
         Ada.Strings.Unbounded.Text_IO.Put (Data_Map_Int.Reference (C).Element.all.ifDesc);
         Ada.Text_IO.Put_Line (Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifAdminStatus'Image        & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifOperStatus'Image         & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifLastChange'Image         & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifInOctets'Image           & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifInDiscards'Image         & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifInErrors'Image           & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifInUnknownProtos'Image    & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifOutOctets'Image          & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifOutDiscards'Image        & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifOutErrors'Image          & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifInMulticastPkts'Image    & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifInBroadcastPkts'Image    & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifOutMulticastPkts'Image   & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifOutBroadcastPkts'Image   & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifHCInOctets'Image         & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifHCInUcastPkts'Image      & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifHCInMulticastPkts'Image  & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifHCInBroadcastPkts'Image  & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifHCOutOctets'Image        & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifHCOutUcastPkts'Image     & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifHCOutMulticastPkts'Image & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifHCOutBroadcastPkts'Image & Ada.Characters.Latin_1.HT &
                                 Data_Map_Int.Reference (C).Element.all.ifCounterDiscontinuityTime'Image
                              );
      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("ifIndex"                                  & Ada.Characters.Latin_1.HT &
                              "adslLineCoding"                         & Ada.Characters.Latin_1.HT &
                              "adslLineType"                           & Ada.Characters.Latin_1.HT &
                              "adslAtucInvVendorID"                    & Ada.Characters.Latin_1.HT &
                              "adslAtucInvVersionNumber"               & Ada.Characters.Latin_1.HT &
                              "adslAtucCurrSnrMgn"                     & Ada.Characters.Latin_1.HT &
                              "adslAtucCurrAtn"                        & Ada.Characters.Latin_1.HT &
                              "adslAtucCurrOutputPwr"                  & Ada.Characters.Latin_1.HT &
                              "adslAtucCurrAttainableRate"             & Ada.Characters.Latin_1.HT &
                              "adslAturInvSerialNumber"                & Ada.Characters.Latin_1.HT &
                              "adslAturInvVendorID"                    & Ada.Characters.Latin_1.HT &
                              "adslAturInvVersionNumber"               & Ada.Characters.Latin_1.HT &
                              "adslAturCurrSnrMgn"                     & Ada.Characters.Latin_1.HT &
                              "adslAturCurrAtn"                        & Ada.Characters.Latin_1.HT &
                              "adslAturCurrOutputPwr"                  & Ada.Characters.Latin_1.HT &
                              "adslAturCurrAttainableRate"             & Ada.Characters.Latin_1.HT &
                              "adslAtucChanCurrTxRate"                 & Ada.Characters.Latin_1.HT &
                              "adslAturChanCurrTxRate"                 & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfLofs"                       & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfLoss"                       & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfLprs"                       & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfESs"                        & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfInits"                      & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr15MinLofs"              & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr15MinLoss"              & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr15MinLprs"              & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr15MinESs"               & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr15MinInits"             & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr1DayLofs"               & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr1DayLoss"               & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr1DayLprs"               & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr1DayESs"                & Ada.Characters.Latin_1.HT &
                              "adslAtucPerfCurr1DayInits"              & Ada.Characters.Latin_1.HT &
                              "adslAturPerfLofs"                       & Ada.Characters.Latin_1.HT &
                              "adslAturPerfLoss"                       & Ada.Characters.Latin_1.HT &
                              "adslAturPerfLprs"                       & Ada.Characters.Latin_1.HT &
                              "adslAturPerfESs"                        & Ada.Characters.Latin_1.HT &
                              "adslAturPerfCurr15MinLofs"              & Ada.Characters.Latin_1.HT &
                              "adslAturPerfCurr15MinLoss"              & Ada.Characters.Latin_1.HT &
                              "adslAturPerfCurr15MinLprs"              & Ada.Characters.Latin_1.HT &
                              "adslAturPerfCurr15MinESs"               & Ada.Characters.Latin_1.HT &
                              "adslAturPerfCurr1DayLofs"               & Ada.Characters.Latin_1.HT &
                              "adslAturPerfCurr1DayLoss"               & Ada.Characters.Latin_1.HT &
                              "adslAturPerfCurr1DayLprs"               & Ada.Characters.Latin_1.HT &
                              "adslAturPerfCurr1DayESs"                & Ada.Characters.Latin_1.HT &
                              "adslAtucChanCorrectedBlks"              & Ada.Characters.Latin_1.HT &
                              "adslAtucChanUncorrectBlks"              & Ada.Characters.Latin_1.HT &
                              "adslAtucChanPerfCurr15MinCorrectedBlks" & Ada.Characters.Latin_1.HT &
                              "adslAtucChanPerfCurr15MinUncorrectBlks" & Ada.Characters.Latin_1.HT &
                              "adslAtucChanPerfCurr1DayCorrectedBlks"  & Ada.Characters.Latin_1.HT &
                              "adslAtucChanPerfCurr1DayUncorrectBlks"  & Ada.Characters.Latin_1.HT &
                              "adslAturChanCorrectedBlks"              & Ada.Characters.Latin_1.HT &
                              "adslAturChanUncorrectBlks"              & Ada.Characters.Latin_1.HT &
                              "adslAturChanPerfCurr15MinCorrectedBlks" & Ada.Characters.Latin_1.HT &
                              "adslAturChanPerfCurr15MinUncorrectBlks" & Ada.Characters.Latin_1.HT &
                              "adslAturChanPerfCurr1DayCorrectedBlks"  & Ada.Characters.Latin_1.HT &
                              "adslAturChanPerfCurr1DayUncorrectBlks"
                           );

      for C in Data_Map_ADSL.Iterate loop
         Ada.Text_IO.Put (Data_Map_ADSL.Reference (C).Element.all.ifIndex'Image & Ada.Characters.Latin_1.HT &
                            Data_Map_ADSL.Reference (C).Element.all.adslLineCoding'Image                         & Ada.Characters.Latin_1.HT &
                            Data_Map_ADSL.Reference (C).Element.all.adslLineType'Image                           & Ada.Characters.Latin_1.HT
                         );
         Ada.Strings.Unbounded.Text_IO.Put (Data_Map_ADSL.Reference (C).Element.all.adslAtucInvVendorID);
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
         Ada.Strings.Unbounded.Text_IO.Put (Data_Map_ADSL.Reference (C).Element.all.adslAtucInvVersionNumber);
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
         Ada.Text_IO.Put      (Data_Map_ADSL.Reference (C).Element.all.adslAtucCurrSnrMgn'Image                    & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucCurrAtn'Image                       & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucCurrOutputPwr'Image                 & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucCurrAttainableRate'Image            & Ada.Characters.Latin_1.HT);
         Ada.Strings.Unbounded.Text_IO.Put (Data_Map_ADSL.Reference (C).Element.all.adslAturInvSerialNumber);
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
         Ada.Strings.Unbounded.Text_IO.Put (Data_Map_ADSL.Reference (C).Element.all.adslAturInvVendorID);
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
         Ada.Strings.Unbounded.Text_IO.Put (Data_Map_ADSL.Reference (C).Element.all.adslAturInvVersionNumber);
         Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
         Ada.Text_IO.Put_Line (Data_Map_ADSL.Reference (C).Element.all.adslAturCurrSnrMgn'Image                     & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturCurrAtn'Image                        & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturCurrOutputPwr'Image                  & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturCurrAttainableRate'Image             & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucChanCurrTxRate'Image                 & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturChanCurrTxRate'Image                 & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfLofs'Image                       & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfLoss'Image                       & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfLprs'Image                       & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfESs'Image                        & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfInits'Image                      & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr15MinLofs'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr15MinLoss'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr15MinLprs'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr15MinESs'Image               & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr15MinInits'Image             & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr1DayLofs'Image               & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr1DayLoss'Image               & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr1DayLprs'Image               & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr1DayESs'Image                & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucPerfCurr1DayInits'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfLofs'Image                       & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfLoss'Image                       & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfLprs'Image                       & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfESs'Image                        & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfCurr15MinLofs'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfCurr15MinLoss'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfCurr15MinLprs'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfCurr15MinESs'Image               & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfCurr1DayLofs'Image               & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfCurr1DayLoss'Image               & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfCurr1DayLprs'Image               & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturPerfCurr1DayESs'Image                & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucChanCorrectedBlks'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucChanUncorrectBlks'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucChanPerfCurr15MinCorrectedBlks'Image & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucChanPerfCurr15MinUncorrectBlks'Image & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucChanPerfCurr1DayCorrectedBlks'Image  & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAtucChanPerfCurr1DayUncorrectBlks'Image  & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturChanCorrectedBlks'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturChanUncorrectBlks'Image              & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturChanPerfCurr15MinCorrectedBlks'Image & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturChanPerfCurr15MinUncorrectBlks'Image & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturChanPerfCurr1DayCorrectedBlks'Image  & Ada.Characters.Latin_1.HT &
                                 Data_Map_ADSL.Reference (C).Element.all.adslAturChanPerfCurr1DayUncorrectBlks'Image
                              );
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Print;
end Print_SNMP;
