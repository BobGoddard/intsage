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

with Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package SNMP_Defines is
   subtype OID is Interfaces.C.unsigned_long;

   SNMP_VERSION_1  : constant := 0;
   SNMP_VERSION_2c : constant := 1;
   SNMP_VERSION_3  : constant := 3;

   SNMP_SEC_LEVEL_NOAUTH     : constant := 1;
   SNMP_SEC_LEVEL_AUTHNOPRIV : constant := 2;
   SNMP_SEC_LEVEL_AUTHPRIV   : constant := 3;

   USM_AUTH_KU_LEN : constant := 32;
   USM_PRIV_KU_LEN : constant := 32;

   SNMP_MSG_GET      : constant := 160;
   SNMP_MSG_GETNEXT  : constant := 161;
   SNMP_MSG_RESPONSE : constant := 162;
   SNMP_MSG_SET      : constant := 163;
   SNMP_MSG_TRAP     : constant := 164;
   SNMP_MSG_GETBULK  : constant := 165;
   SNMP_MSG_INFORM   : constant := 166;
   SNMP_MSG_TRAP2    : constant := 167;
   SNMP_MSG_REPORT   : constant := 168;

   STAT_SUCCESS        : constant := 0;
   STAT_ERROR          : constant := 1;
   STAT_TIMEOUT        : constant := 2;

   SNMP_ERR_NOERROR    : constant := 0;
   SNMP_ERR_TOOBIG     : constant := 1;
   SNMP_ERR_NOSUCHNAME : constant := 2;
   SNMP_ERR_BADVALUE   : constant := 3;
   SNMP_ERR_READONLY   : constant := 4;
   SNMP_ERR_GENERR     : constant := 5;
   MAX_OID_LEN         : constant := 128;

   SNMP_NOSUCHOBJECT   : constant := 128;
   SNMP_NOSUCHINSTANCE : constant := 129;
   SNMP_ENDOFMIBVIEW   : constant := 130;

   ASN_BOOLEAN                               : constant Interfaces.C.unsigned_char := Interfaces.C.unsigned_char (16#01#);
   NETSNMP_DS_APPLICATION_ID                 : constant Interfaces.C.int := 1;
   NETSNMP_DS_WALK_INCLUDE_REQUESTED         : constant Interfaces.C.int := 1;
   NETSNMP_DS_WALK_PRINT_STATISTICS          : constant Interfaces.C.int := 2;
   NETSNMP_DS_WALK_DONT_CHECK_LEXICOGRAPHIC  : constant Interfaces.C.int := 3;

   type SecurityAuthKey_Array is new Interfaces.C.Strings.chars_ptr_array (0 .. 31);
   type SecurityPrivKey_Array is array (0 .. 31) of aliased Interfaces.C.unsigned_char;
   type OID_Array             is array (Positive range <>) of OID;
   type OID_Max_Array         is array (1 .. MAX_OID_LEN)  of OID;
   pragma Convention (C, OID_Array);
   pragma Convention (C, OID_Max_Array);

   type Counter64 is record
      High : aliased Interfaces.C.unsigned_long;
      Low  : aliased Interfaces.C.unsigned_long;
   end record;

   type NetSNMP_VarData (Discr : unsigned := 0) is record
      case Discr is
         when 0 => integer            : access Interfaces.C.long;
         when 1 => string             :        Interfaces.C.Strings.chars_ptr;
         when 2 => objid              : access Interfaces.C.unsigned_long;
         when 3 => bitstring          : access Interfaces.C.unsigned_char;
         when others => the_counter64 : access Counter64;
      end case;
   end record;
   pragma Convention (C, NetSNMP_VarData);
   pragma Unchecked_Union (NetSNMP_VarData);

   type Name_Loc_Array is array (0 .. 127) of aliased Interfaces.C.unsigned_long;
   type Buf_Array      is array (0 .. 39) of aliased Interfaces.C.unsigned_char;
   type Variable_List is record
      Next_Variable : access Variable_List;
      Name          : access OID_Max_Array;
      Name_Length   : aliased Interfaces.C.size_t;
      C_Type        : aliased Interfaces.C.unsigned_char;
      Val           : aliased NetSNMP_VarData;
      Val_Len       : aliased Interfaces.C.size_t;
      Name_Loc      : aliased Name_Loc_Array;
      Buf           : aliased Buf_Array;
      Data          : System.Address;
      DataFreeHook  : access procedure (arg1 : System.Address);
      Index         : aliased Interfaces.C.int;
   end record;
   pragma Convention (C, Variable_List);
   subtype NetSNMP_Variable_List is Variable_List;

   type Agent_Addr_Array is array (0 .. 3) of aliased Interfaces.C.unsigned_char;
   type SNMP_PDU is record
      Version               : aliased Interfaces.C.long;
      Command               : aliased Interfaces.C.int;
      ReqID                 : aliased Interfaces.C.long;
      MsgID                 : aliased Interfaces.C.long;
      TransID               : aliased Interfaces.C.long;
      SessID                : aliased Interfaces.C.long;
      ErrStat               : aliased Interfaces.C.long;
      ErrIndex              : aliased Interfaces.C.long;
      Time                  : aliased Interfaces.C.unsigned_long;
      Flags                 : aliased Interfaces.C.unsigned_long;
      SecurityModel         : aliased Interfaces.C.int;
      SecurityLevel         : aliased Interfaces.C.int;
      MsgParseModel         : aliased Interfaces.C.int;
      Transport_Data        :         System.Address;
      Transport_Data_Dength : aliased Interfaces.C.int;
      TDomain               : access  Interfaces.C.unsigned_long;
      TDomainLen            : aliased Interfaces.C.size_t;
      Variables             : access  NetSNMP_Variable_List;
      Community             : access  Interfaces.C.unsigned_char;
      Community_Len         : aliased Interfaces.C.size_t;
      Enterprise            : access  Interfaces.C.unsigned_long;
      Enterprise_Length     : aliased Interfaces.C.size_t;
      Trap_Type             : aliased Interfaces.C.long;
      Specific_Type         : aliased Interfaces.C.long;
      Agent_Sddr            : aliased Agent_Addr_Array;
      ContextEngineID       : access  Interfaces.C.unsigned_char;
      ContextEngineIDLen    : aliased Interfaces.C.size_t;
      ContextName           :         Interfaces.C.Strings.chars_ptr;
      ContextNameLen        : aliased Interfaces.C.size_t;
      SecurityEngineID      : access  Interfaces.C.unsigned_char;
      SecurityEngineIDLen   : aliased Interfaces.C.size_t;
      SecurityName          :         Interfaces.C.Strings.chars_ptr;
      SecurityNameLen       : aliased Interfaces.C.size_t;
      Priority              : aliased Interfaces.C.int;
      Range_SubID           : aliased Interfaces.C.int;
      SecurityStateRef      :         System.Address;
   end record;
   pragma Convention (C, SNMP_PDU);
   subtype NetSNMP_PDU is SNMP_PDU;

   type SNMP_Session;
   type SNMP_Session_Access is access SNMP_Session;

   type NetSNMP_CallBack is access function
     (Arg1 :        Interfaces.C.int;
      Arg2 :        SNMP_Session_Access;
      Arg3 :        Interfaces.C.int;
      Arg4 : access NetSNMP_PDU;
      Arg5 :        System.Address) return Interfaces.C.int;
   pragma Convention (C, NetSNMP_CallBack);

   --  sizeof (snmp_session) - 416
   type snmp_Session is record
      Version                 : aliased Interfaces.C.long;
      Retries                 : aliased Interfaces.C.int;
      Timeout                 : aliased Interfaces.C.long;
      Flags                   : aliased Interfaces.C.unsigned_long;
      Subsession              : access  SNMP_Session;
      Next                    : access  SNMP_Session;
      PeerName                :         Interfaces.C.Strings.chars_ptr;
      Remote_Port             : aliased Interfaces.C.unsigned_short;
      LocalName               :         Interfaces.C.Strings.chars_ptr;
      Local_Port              : aliased Interfaces.C.unsigned_short;
      Authenticator           : access function
        (Arg1                    : access  Interfaces.C.unsigned_char;
         Arg2                    : access  Interfaces.C.size_t;
         Arg3                    : access  Interfaces.C.unsigned_char;
         Arg4                    :         Interfaces.C.size_t) return access Interfaces.C.unsigned_char;
      CallBack                :         NetSNMP_CallBack;
      CallBack_Magic          :         System.Address;
      S_ErrNo                 : aliased Interfaces.C.int;
      S_SNMP_ErrNo            : aliased Interfaces.C.int;
      Sessid                  : aliased Interfaces.C.long;
      Community               : access  Interfaces.C.unsigned_char;
      Community_Len           : aliased Interfaces.C.size_t;
      RcvMsgMaxSize           : aliased Interfaces.C.size_t;
      SndMsgMaxSize           : aliased Interfaces.C.size_t;
      IsAuthoritative         : aliased Interfaces.C.unsigned_char;
      ContextEngineID         : access  Interfaces.C.unsigned_char;
      ContextEngineIDLen      : aliased Interfaces.C.size_t;
      EngineBoots             : aliased Interfaces.C.unsigned;
      EngineTime              : aliased Interfaces.C.unsigned;
      ContextName             :         Interfaces.C.Strings.chars_ptr;
      ContextNameLen          : aliased Interfaces.C.size_t;
      SecurityEngineID        : access  Interfaces.C.unsigned_char;
      SecurityEngineIDLen     : aliased Interfaces.C.size_t;
      SecurityName            :         Interfaces.C.Strings.chars_ptr;
      SecurityNameLen         : aliased Interfaces.C.size_t;
      SecurityAuthProto       : System.Address;
      SecurityAuthProtoLen    : aliased Interfaces.C.size_t; --  good
      SecurityAuthKey         : aliased SecurityPrivKey_Array;
      SecurityAuthKeyLen      : aliased Interfaces.C.size_t; -- ugh
      SecurityAuthLocalKey    : access  Interfaces.C.unsigned_char;
      SecurityAuthLocalKeyLen : aliased Interfaces.C.size_t;
      SecurityPrivProto       : access  Interfaces.C.unsigned_long; --  ?????????????        securityPrivProto : access net_snmp_library_oid_h.oid;  -- /usr/include/net-snmp/types.h:380
      SecurityPrivProtoLen    : aliased Interfaces.C.size_t;
      SecurityPrivKey         : aliased SecurityPrivKey_Array;
      SecurityPrivKeyLen      : aliased Interfaces.C.size_t;
      SecurityPrivLocalKey    : access  Interfaces.C.unsigned_char;
      SecurityPrivLocalKeyLen : aliased Interfaces.C.size_t;
      SecurityModel           : aliased Interfaces.C.int;
      SecurityLevel           : aliased Interfaces.C.int;
      ParamName               :         Interfaces.C.Strings.chars_ptr;
      SecurityInfo            :         System.Address;
      Transport_Configuration :         System.Address;
      MyVoid                  :         System.Address;
   end record;
   pragma Convention (C, SNMP_Session);
   subtype NetSNMP_Session is SNMP_Session;

   function local_Generate_Ku (S : SNMP_Session; P : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   pragma Import (C, local_Generate_Ku, "local_generate_Ku");

   function Generate_Ku (HashType     : System.Address;
                         HashType_Len : Interfaces.C.size_t;
                         Password     : Interfaces.C.Strings.chars_ptr;
                         PasswordLen  : Interfaces.C.size_t;
                         Ku           : aliased SecurityPrivKey_Array;
                         KuLen        : access Interfaces.C.size_t
                        ) return Interfaces.C.int;
   pragma Import (C, Generate_Ku, "generate_Ku");

   procedure Init_SNMP (Name : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Init_SNMP, "init_snmp");

   function NetSNMP_Get_DS_Boolean (Storeid : Interfaces.C.int; Which : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, NetSNMP_Get_DS_Boolean, "netsnmp_ds_get_boolean");

   function NetSNMP_DS_Register_Config (RType : Interfaces.C.unsigned_char; FType : Interfaces.C.Strings.chars_ptr; Token : Interfaces.C.Strings.chars_ptr; StoreID : Interfaces.C.int; Which : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, NetSNMP_DS_Register_Config, "netsnmp_ds_register_config");
   --   netsnmp_ds_register_config(u_char type, const char *ftype, const char *token, int storeid, int which);

   procedure Print_SNMP_Variable (O : System.Address; L : Interfaces.C.size_t; V : variable_list);
   pragma Import (C, Print_SNMP_Variable, "print_variable");
--       void  print_variable(const oid *objid, size_t objidlen, const netsnmp_variable_list *variable);

   procedure SNMP_Add_Null_Var (PDU : access SNMP_PDU; OIS : System.Address; Name_Length : Interfaces.C.size_t);
   pragma Import (C, SNMP_Add_Null_Var, "snmp_add_null_var");
   --  snmp_add_null_var(netsnmp_pdu *pdu, const oid * name, size_t name_length);

   function SNMP_Close (Z : access SNMP_Session) return Interfaces.C.int;
   pragma Import (C, SNMP_Close, "snmp_close");

   function SNMP_OID_Compare (LO : System.Address; LO_Len : Interfaces.C.size_t; LR : System.Address; LR_Len : Interfaces.C.size_t) return Interfaces.C.int;
   pragma Import (C, SNMP_OID_Compare, "snmp_oid_compare");
--    int             snmp_oid_compare(const oid *, size_t, const oid *, size_t);

   procedure SNMP_Free_PDU (PDU : access SNMP_PDU);
   pragma Import (C, SNMP_Free_PDU, "snmp_free_pdu");

   function SNMP_Open (z : access SNMP_Session) return access SNMP_Session;
   pragma Import (C, SNMP_Open, "snmp_open");

   function SNMP_PDU_Create (z : Interfaces.C.int) return access SNMP_PDU;
   pragma Import (C, SNMP_PDU_Create, "snmp_pdu_create");

   procedure SNMP_Sess_Init (Session : System.Address);
   pragma Import (C, SNMP_Sess_Init, "snmp_sess_init");

   --  void snmp_sess_perror (char *msg,netsnmp_session *sess);
   procedure SNMP_Session_PError (Str : Interfaces.C.Strings.chars_ptr; Session : access SNMP_Session);
   pragma Import (C, SNMP_Session_PError, "snmp_sess_perror");

   procedure SNMP_Synch_Response (Session : access SNMP_Session; PDU : access SNMP_PDU; Response : out SNMP_PDU; Res : out Interfaces.C.int);
   pragma Import (C, SNMP_Synch_Response, "local_snmp_synch_response");
--  int             snmp_synch_response(netsnmp_session *, netsnmp_pdu *, netsnmp_pdu **);

   NETSNMP_USMAUTH_BASE_OID : OID_Array (1 .. 9) := (1, 3, 6, 1, 6, 3, 10, 1, 1);
   NETSNMP_USMAUTH_HMACSHA1 : OID := 3;
   usmHMACSHA1AuthProtocol  : OID_Array := (1, 3, 6, 1, 6, 3, 10, 1, 1, 3); --  NETSNMP_USMAUTH_BASE_OID & NETSNMP_USMAUTH_HMACSHA1; { 1,3,6,1,6,3,10,1,1,3 }
end SNMP_Defines;
