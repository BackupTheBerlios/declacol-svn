unit class_ipfilter;
{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
Author: Sven Lorenz / Borg@Sven-of-Nine.de
}

////////////////////////////////////////////////////////////////////////////////
//
//   Unit           :  IPFILTER
//   Author         :  rllibby
//   Date           :  05.25.2004
//   Description    :  TCPIP network connection blocking unit
//
////////////////////////////////////////////////////////////////////////////////
{
A simple demo is given below to demonstrate the usage:

  // Create filter object
  with TIpFilter.Create do
  begin
     // Set active
     Active:=True;
     // Some example filters
     AddOutboundFilter(EmptyStr, protoUdp, 5001); // Filter all outbound udp requests going to port 5001
     AddOutboundFilter(EmptyStr, protoIcmp, FILTER_ICMP_TYPE_ANY); // Filter all outgoing ICMP requests
     AddInboundFilter('www.experts-exchange.com', protoTcp, 80); // Filter any incoming request from EE on port 80
     AddOutboundFilter('www.google.com', protoTcp, 80); // Filter outgoing request to google
     // Free (this will remove all filters)
     Free;
  end;
}
interface

////////////////////////////////////////////////////////////////////////////////
//   Include units
////////////////////////////////////////////////////////////////////////////////
uses
  Windows,
  SysUtils,
  Classes,
  WinSock;

////////////////////////////////////////////////////////////////////////////////
//   IP helper library
////////////////////////////////////////////////////////////////////////////////
const
  IPHLPAPI                      =  'IPHLPAPI.DLL';

////////////////////////////////////////////////////////////////////////////////
//   Data types
////////////////////////////////////////////////////////////////////////////////
type
  PByteArray                    =  ^TByteArray;
  TByteArray                    =  Array [0..Pred(MaxInt)] of Byte;
  PIpBytes                      =  ^TIpBytes;
  TIpBytes                      =  Array [0..3] of Byte;

////////////////////////////////////////////////////////////////////////////////
//   FltDefs translation
////////////////////////////////////////////////////////////////////////////////
type
  FILTER_HANDLE                 =  Pointer;
  PFILTER_HANDLE                =  ^FILTER_HANDLE;
  INTERFACE_HANDLE              =  Pointer;
  PINTERFACE_HANDLE             =  ^INTERFACE_HANDLE;

const
  GF_FRAGMENTS                  =  2;
  GF_STRONGHOST                 =  8;
  GF_FRAGCACHE                  =  9;

type
  GLOBAL_FILTER                 =  Integer;
  PGLOBAL_FILTER                =  ^GLOBAL_FILTER;

const
  PF_IPV4                       =  0;
  PF_IPV6                       =  1;

type
  PFADDRESSTYPE                 =  Integer;
  PPFADDRESSTYPE                =  ^PFADDRESSTYPE;

const
  PF_ACTION_FORWARD             =  0;
  PF_ACTION_DROP                =  1;

type
  PFFORWARD_ACTION              =  Integer;
  PPFFORWARD_ACTION             =  ^PPFFORWARD_ACTION;

const
  PFFT_FILTER                   =  1;
  PFFT_FRAG                     =  2;
  PFFT_SPOOF                    =  3;

type
  PFFRAMETYPE                   =  Integer;
  PPFFRAMETYPE                  =  ^PFFRAMETYPE;

type
  _PF_FILTER_DESCRIPTOR         =  packed record
     dwFilterFlags:             DWORD;
     dwRule:                    DWORD;
     pfatType:                  PFADDRESSTYPE;
     SrcAddr:                   PIpBytes;
     SrcMask:                   PIpBytes;
     DstAddr:                   PIpBytes;
     DstMask:                   PIpBytes;
     dwProtocol:                DWORD;
     fLateBound:                DWORD;
     wSrcPort:                  Word;
     wDstPort:                  Word;
     wSrcPortHighRange:         Word;
     wDstPortHighRange:         Word;
  end;
  PF_FILTER_DESCRIPTOR          =  _PF_FILTER_DESCRIPTOR;
  PPF_FILTER_DESCRIPTOR         =  ^PF_FILTER_DESCRIPTOR;

type
  _PF_FILTER_STATS              =  packed record
     dwNumPacketsFiltered:      DWORD;
     info:                      PF_FILTER_DESCRIPTOR;
  end;
  PF_FILTER_STATS               =  _PF_FILTER_STATS;
  PPF_FILTER_STATS              =  ^PF_FILTER_STATS;

type
  _PF_INTERFACE_STATS           =  packed record
     pvDriverContext:           Pointer;
     dwFlags:                   DWORD;
     dwInDrops:                 DWORD;
     dwOutDrops:                DWORD;
     eaInAction:                PFFORWARD_ACTION;
     eaOutAction:               PFFORWARD_ACTION;
     dwNumInFilters:            DWORD;
     dwNumOutFilters:           DWORD;
     dwFrag:                    DWORD;
     dwSpoof:                   DWORD;
     dwReserved1:               DWORD;
     dwReserved2:               DWORD;
     liSyn:                     LARGE_INTEGER;
     liTotalLogged:             LARGE_INTEGER;
     dwLostLogEntries:          DWORD;
     FilterInfo:                Array [0..0] of PF_FILTER_STATS;
  end;
  PF_INTERFACE_STATS            =  _PF_INTERFACE_STATS;
  PPF_INTERFACE_STATS           =  ^PF_INTERFACE_STATS;

type
  _PF_LATEBIND_INFO             =  packed record
     SrcAddr:                   PByteArray;
     DstAddr:                   PByteArray;
     Mask:                      PByteArray;
  end;
  PF_LATEBIND_INFO              =  _PF_LATEBIND_INFO;
  PPF_LATEBIND_INFO             =  ^PF_LATEBIND_INFO;

type
  _PFLOGFRAME                   =  packed record
     Timestamp:                 LARGE_INTEGER;
     pfeTypeOfFrame:            PFFRAMETYPE;
     dwTotalSizeUsed:           DWORD;
     dwFilterRule:              DWORD;
     wSizeOfAdditionalData:     Word;
     wSizeOfIpHeader:           Word;
     dwInterfaceName:           DWORD;
     dwIPIndex:                 DWORD;
     bPacketData:               Array [0..0] of Byte;
  end;
  PFLOGFRAME                    =  _PFLOGFRAME;
  PPFLOGFRAME                   =  ^PFLOGFRAME;

const
  FILTER_PROTO_ANY              =  $00;
  FILTER_PROTO_ICMP             =  $01;
  FILTER_PROTO_TCP              =  $06;
  FILTER_PROTO_UDP              =  $11;
  FILTER_TCPUDP_PORT_ANY        =  $00;

const
  FILTER_ICMP_TYPE_ANY          =  $FF;
  FILTER_ICMP_CODE_ANY          =  $FF;

const
  FD_FLAGS_NOSYN                =  $01;
  FD_FLAGS_ALLFLAGS             =  FD_FLAGS_NOSYN;

const
  LB_SRC_ADDR_USE_SRCADDR_FLAG  =  $00000001;
  LB_SRC_ADDR_USE_DSTADDR_FLAG  =  $00000002;
  LB_DST_ADDR_USE_SRCADDR_FLAG  =  $00000004;
  LB_DST_ADDR_USE_DSTADDR_FLAG  =  $00000008;
  LB_SRC_MASK_LATE_FLAG         =  $00000010;
  LB_DST_MASK_LATE_FLAG         =  $00000020;

const
  ERROR_BASE                    =  23000;
  PFERROR_NO_PF_INTERFACE       =  (ERROR_BASE + 0); // never returned.
  PFERROR_NO_FILTERS_GIVEN      =  (ERROR_BASE + 1);
  PFERROR_BUFFER_TOO_SMALL      =  (ERROR_BASE + 2);
  ERROR_IPV6_NOT_IMPLEMENTED    =  (ERROR_BASE + 3);

function   PfCreateInterface(
           dwName:              DWORD;
           inAction:            PFFORWARD_ACTION;
           outAction:           PFFORWARD_ACTION;
           bUseLog:             BOOL;
           bMustBeUnique:       BOOL;
           var ppInterface:     INTERFACE_HANDLE): DWORD;
           stdcall; external IPHLPAPI name '_PfCreateInterface@24';

function   PfDeleteInterface(
           pInterface:          INTERFACE_HANDLE): DWORD;
           stdcall; external IPHLPAPI name '_PfDeleteInterface@4';

function   PfAddFiltersToInterface(
           ih:                  INTERFACE_HANDLE;
           cInFilters:          DWORD;
           pfiltIn:             PPF_FILTER_DESCRIPTOR;
           cOutFilters:         DWORD;
           pfiltOut:            PPF_FILTER_DESCRIPTOR;
           pfHandle:            PFILTER_HANDLE): DWORD;
           stdcall; external IPHLPAPI name '_PfAddFiltersToInterface@24';

function   PfRemoveFiltersFromInterface(
           ih:                  INTERFACE_HANDLE;
           cInFilters:          DWORD;
           pfiltIn:             PPF_FILTER_DESCRIPTOR;
           cOutFilters:         DWORD;
           pfiltOut:            PPF_FILTER_DESCRIPTOR): DWORD;
           stdcall; external IPHLPAPI name '_PfRemoveFiltersFromInterface@20';

function   PfRemoveFilterHandles(
           pInterface:          INTERFACE_HANDLE;
           cFilters:            DWORD;
           pvHandles:           PFILTER_HANDLE): DWORD;
           stdcall; external IPHLPAPI name '_PfRemoveFilterHandles@12';

function   PfUnBindInterface(
           pInterface:          INTERFACE_HANDLE): DWORD;
           stdcall; external IPHLPAPI name '_PfUnBindInterface@4';

function   PfBindInterfaceToIndex(
           pInterface:          INTERFACE_HANDLE;
           dwIndex:             DWORD;
           pfatLinkType:        PFADDRESSTYPE;
           LinkIPAddress:       PByteArray): DWORD;
           stdcall; external IPHLPAPI name '_PfBindInterfaceToIndex@16';

function   PfBindInterfaceToIPAddress(
           pInterface:          INTERFACE_HANDLE;
           pfatLinkType:        PFADDRESSTYPE;
           IPAddress:           PByteArray): DWORD;
           stdcall; external IPHLPAPI name '_PfBindInterfaceToIPAddress@12';

function   PfRebindFilters(
           pInterface:          INTERFACE_HANDLE;
           pLateBindInfo:       PPF_LATEBIND_INFO): DWORD;
           stdcall; external IPHLPAPI name '_PfRebindFilters@8';

function   PfAddGlobalFilterToInterface(
           pInterface:          INTERFACE_HANDLE;
           gfFilter:            GLOBAL_FILTER): DWORD;
           stdcall; external IPHLPAPI name '_PfAddGlobalFilterToInterface@8';

function   PfRemoveGlobalFilterFromInterface(
           pInterface:          INTERFACE_HANDLE;
           gfFilter:            GLOBAL_FILTER): DWORD;
           stdcall; external IPHLPAPI name '_PfRemoveGlobalFilterFromInterface@8';

function   PfMakeLog(
           hEvent:              THandle): DWORD;
           stdcall; external IPHLPAPI name '_PfMakeLog@4';

function   PfSetLogBuffer(
           pbBuffer:            PByteArray;
           dwSize:              DWORD;
           dwThreshold:         DWORD;
           dwEntries:           DWORD;
           pdwLoggedEntries:    PDWORD;
           pdwLostEntries:      PDWORD;
           pdwSizeUsed:         PDWORD): DWORD;
           stdcall; external IPHLPAPI name '_PfSetLogBuffer@28';

function   PfDeleteLog: DWORD;
           stdcall; external IPHLPAPI name '_PfDeleteLog@0';

function   PfGetInterfaceStatistics(
           pInterface:          INTERFACE_HANDLE;
           ppfStats:            PPF_INTERFACE_STATS;
           pdwBufferSize:       PDWORD;
           fResetCounters:      BOOL): DWORD;
           stdcall; external IPHLPAPI name '_PfGetInterfaceStatistics@16';

function   PfTestPacket(
           pInInterface:        INTERFACE_HANDLE;
           pOutInterface:       INTERFACE_HANDLE;
           cBytes:              DWORD;
           pbPacket:            PByteArray;
           ppAction:            PPFFORWARD_ACTION): DWORD;
           stdcall; external IPHLPAPI name '_PfTestPacket@20';

////////////////////////////////////////////////////////////////////////////////
//   IP filter class wrapper
////////////////////////////////////////////////////////////////////////////////
const
  IP_LOCALHOST      =  'localhost';
  IP_MASKALL        =  '0.0.0.0';
  IP_MASKNONE       =  '255.255.255.255';

type
  TIpProtocol       =  (protoAny, protoIcmp, protoTcp, protoUdp);
  TIpFilter         =  class(TObject)
  private
     // Private declarations
     FHandle:       INTERFACE_HANDLE;
     FLocalIP:      TIpBytes;
     FActive:       Boolean;
     FOutbound:     TList;
     FInbound:      TList;
     function       AllocIpBytes(Address: String): PIpBytes;
     function       AllocFilter(Protocol: TIpProtocol): PPF_FILTER_DESCRIPTOR;
     procedure      FreeFilter(Filter: PPF_FILTER_DESCRIPTOR);
  protected
     // Protected declarations
     function       GetInboundFilter(Index: Integer): PF_FILTER_DESCRIPTOR;
     function       GetInboundFilterCount: Integer;
     function       GetOutboundFilter(Index: Integer): PF_FILTER_DESCRIPTOR;
     function       GetOutboundFilterCount: Integer;
     function       Startup: Boolean;
     procedure      SetActive(Value: Boolean);
     procedure      Cleanup;
  public
     // Public declarations
     constructor    Create;
     destructor     Destroy; override;
     function       AddInboundFilter(Address: String; Protocol: TIpProtocol; Port: Word): Integer;
     function       AddOutboundFilter(Address: String; Protocol: TIpProtocol; Port: Word): Integer;
     procedure      ClearInboundFilters;
     procedure      ClearOutboundFilters;
     procedure      DeleteInboundFilter(Index: Integer);
     procedure      DeleteOutboundFilter(Index: Integer);
     property       Active: Boolean read FActive write SetActive;
     property       InterfaceHandle: INTERFACE_HANDLE read FHandle;
     property       InboundFilterCount: Integer read GetInboundFilterCount;
     property       InboundFilters[Index: Integer]: PF_FILTER_DESCRIPTOR read GetInboundFilter;
     property       OutboundFilterCount: Integer read GetOutboundFilterCount;
     property       OutboundFilters[Index: Integer]: PF_FILTER_DESCRIPTOR read GetOutboundFilter;
  end;

////////////////////////////////////////////////////////////////////////////////
//   Utility functions
////////////////////////////////////////////////////////////////////////////////
function   StrToIp(lpszIP: PChar; lpipAddr: PIpBytes): PIpBytes;
function   GetLocalIPAddr(lpipAddr: PIpBytes): Boolean;

implementation

////////////////////////////////////////////////////////////////////////////////
//   Protected variables
////////////////////////////////////////////////////////////////////////////////
var
  wsaData:       TWSAData;

////////////////////////////////////////////////////////////////////////////////
//   TIpFilter
////////////////////////////////////////////////////////////////////////////////
procedure TIpFilter.ClearInboundFilters;
var  dwIndex:       Integer;
begin

  // Walk all filters and remove them
  for dwIndex:=Pred(FInbound.Count) downto 0 do DeleteInboundFilter(dwIndex);

end;

procedure TIpFilter.ClearOutboundFilters;
var  dwIndex:       Integer;
begin

  // Walk all filters and remove them
  for dwIndex:=Pred(FOutbound.Count) downto 0 do DeleteOutboundFilter(dwIndex);

end;

procedure TIpFilter.DeleteInboundFilter(Index: Integer);
var  lpFilter:      PPF_FILTER_DESCRIPTOR;
begin

  // Get the filter from the list
  lpFilter:=FInbound[Index];

  // Resource protection
  try
     // Remove filter from the list
     FInbound.Delete(Index);
     // Remove the filter
     Win32Check(PfRemoveFiltersFromInterface(FHandle, 1, lpFilter, 0, nil) = NO_ERROR);
  finally
     // Free the filter
     FreeFilter(lpFilter);
  end;

end;

procedure TIpFilter.DeleteOutboundFilter(Index: Integer);
var  lpFilter:      PPF_FILTER_DESCRIPTOR;
begin

  // Get the filter from the list
  lpFilter:=FOutbound[Index];

  // Resource protection
  try
     // Remove filter from the list
     FOutbound.Delete(Index);
     // Remove the filter
     Win32Check(PfRemoveFiltersFromInterface(FHandle, 0, nil, 1, lpFilter) = NO_ERROR);
  finally
     // Free the filter
     FreeFilter(lpFilter);
  end;

end;

function TIpFilter.GetInboundFilter(Index: Integer): PF_FILTER_DESCRIPTOR;
begin

  // Get the filter descriptor
  result:=PF_FILTER_DESCRIPTOR(FInbound[Index]^);

end;

function TIpFilter.GetOutboundFilter(Index: Integer): PF_FILTER_DESCRIPTOR;
begin

  // Get the filter descriptor
  result:=PF_FILTER_DESCRIPTOR(FOutbound[Index]^);

end;

function TIpFilter.GetInboundFilterCount: Integer;
begin

  // Return filter count
  result:=FInbound.Count;

end;

function TIpFilter.GetOutboundFilterCount: Integer;
begin

  // Return filter count
  result:=FOutbound.Count;

end;

function TIpFilter.AddInboundFilter(Address: String; Protocol: TIpProtocol; Port: Word): Integer;
var  lpFilter:      PPF_FILTER_DESCRIPTOR;
begin

  // Allocate memory for filter
  lpFilter:=AllocFilter(Protocol);

  // Resource protection
  try
     // Check protocol
     if (Protocol = protoIcmp) then
     begin
        // Icmp handling
        lpFilter^.wDstPort:=FILTER_ICMP_CODE_ANY;
        lpFilter^.wDstPortHighRange:=FILTER_ICMP_CODE_ANY;
     end
     else
     begin
        // Tcp/Udp handling
        lpFilter^.wDstPort:=FILTER_TCPUDP_PORT_ANY;
        lpFilter^.wDstPortHighRange:=FILTER_TCPUDP_PORT_ANY;
     end;
     // Note: UDP number needs to be decremented by one (ie 5000 blocks 5001)
     if (Protocol = protoUdp) and (Port > 0) then Dec(Port);
     lpFilter^.wSrcPort:=Port;
     lpFilter^.wSrcPortHighRange:=Port;
     // Check remote address
     if (Length(Address) = 0)then
     begin
        // Filter from all remote addresses
        lpFilter^.SrcAddr:=AllocIpBytes(IP_MASKALL);
        lpFIlter^.SrcMask:=AllocIpBytes(IP_MASKALL);
     end
     else
     begin
        // Filter from specific remote address
        lpFilter^.SrcAddr:=AllocIpBytes(Address);
        lpFilter^.SrcMask:=AllocIpBytes(IP_MASKNONE);
     end;
     // Set destination address
     lpFilter^.DstAddr:=AllocIpBytes(IP_LOCALHOST);
     lpFilter^.DstMask:=AllocIpBytes(IP_MASKNONE);
     // Add filter to interface
     Win32Check(PfAddFiltersToInterface(FHandle, 1, lpFilter, 0, nil, nil) = NO_ERROR);
  finally
     // Add filter to list
     result:=FInbound.Add(lpFilter);
  end;

end;

function TIpFilter.AddOutboundFilter(Address: String; Protocol: TIpProtocol; Port: Word): Integer;
var  lpFilter:      PPF_FILTER_DESCRIPTOR;
begin

  // Allocate memory for filter
  lpFilter:=AllocFilter(Protocol);

  // Resource protection
  try
     // Note: UDP number needs to be decremented by one (ie 5000 blocks 5001)
     if (Protocol = protoUdp) and (Port > 0) then Dec(Port);
     // Filter all remote ports coming into specified port
     lpFilter^.wDstPort:=Port;
     lpFilter^.wDstPortHighRange:=Port;
     // Check protocol
     if (Protocol = protoIcmp) then
     begin
        // Icmp handling
        lpFilter^.wSrcPort:=FILTER_ICMP_CODE_ANY;
        lpFilter^.wSrcPortHighRange:=FILTER_ICMP_CODE_ANY;
     end
     else
     begin
        // Tcp/Udp handling
        lpFilter^.wSrcPort:=FILTER_TCPUDP_PORT_ANY;
        lpFilter^.wSrcPortHighRange:=FILTER_TCPUDP_PORT_ANY;
     end;
     // Set source address
     lpFilter^.SrcAddr:=AllocIpBytes(IP_LOCALHOST);
     lpFilter^.SrcMask:=AllocIpBytes(IP_MASKNONE);
     // Check remote address
     if (Length(Address) = 0)then
     begin
        // Filter from all remote addresses
        lpFilter^.DstAddr:=AllocIpBytes(IP_MASKALL);
        lpFIlter^.DstMask:=AllocIpBytes(IP_MASKALL);
     end
     else
     begin
        // Filter from specific remote address
        lpFilter^.DstAddr:=AllocIpBytes(Address);
        lpFilter^.DstMask:=AllocIpBytes(IP_MASKNONE);
     end;
     // Add filter to interface
     Win32Check(PfAddFiltersToInterface(FHandle, 0, nil, 1, lpFilter, nil) = NO_ERROR);
  finally
     // Add filter to list
     result:=FOutbound.Add(lpFilter);
  end;

end;

function TIpFilter.AllocIpBytes(Address: String): PIpBytes;
begin

  // Allocate memory for IP byte setting and convert address
  result:=StrToIp(PChar(Address), AllocMem(SizeOf(TIpBytes)));

end;

function TIpFilter.AllocFilter(Protocol: TIpProtocol): PPF_FILTER_DESCRIPTOR;
begin

  // Allocate memory for filter
  result:=AllocMem(SizeOf(PF_FILTER_DESCRIPTOR));

  // Set defaults for all filters
  result^.dwFilterFlags:=FD_FLAGS_NOSYN;
  result^.dwRule:=0;
  result^.pfatType:=PF_IPV4;
  result^.fLateBound:=0;

  // Set protocol filtering
  case Protocol of
     protoAny    :  result^.dwProtocol:=FILTER_PROTO_ANY;
     protoIcmp   :  result^.dwProtocol:=FILTER_PROTO_ICMP;
     protoTcp    :  result^.dwProtocol:=FILTER_PROTO_TCP;
     protoUdp    :  result^.dwProtocol:=FILTER_PROTO_UDP;
  else
     // Sanity check
     result^.dwProtocol:=FILTER_PROTO_ANY;
  end;

end;

procedure TIpFilter.FreeFilter(Filter: PPF_FILTER_DESCRIPTOR);
begin

  // Check filter
  if Assigned(Filter) then
  begin
     // Free memory for addresses and masks
     if Assigned(Filter^.SrcAddr) then FreeMem(Filter.SrcAddr);
     if Assigned(Filter^.SrcMask) then FreeMem(Filter.SrcMask);
     if Assigned(Filter^.DstAddr) then FreeMem(Filter.DstAddr);
     if Assigned(Filter^.DstMask) then FreeMem(Filter.DstMask);
     // Free memory for record struct
     FreeMem(Filter);
  end;

end;

procedure TIpFilter.SetActive(Value: Boolean);
begin

  // Check for state change
  if (Value <> FActive) then
  begin
     // Check old state
     if FActive then
        // Unbind the interface
        Win32Check(PfUnBindInterface(FHandle) = NO_ERROR)
     else
        Win32Check(PfBindInterfaceToIPAddress(FHandle, PF_IPV4, @FLocalIP) = NO_ERROR);
     // Set new state
     FActive:=Value;
  end;

end;

function TIpFilter.Startup: Boolean;
begin

  // Get the local IP address
  GetLocalIPAddr(@FLocalIP);

  // Create the interface (do not bind yet)
  result:=(PfCreateInterface(0, PF_ACTION_FORWARD, PF_ACTION_FORWARD, False, True, FHandle) = NO_ERROR);

end;

procedure TIpFilter.Cleanup;
var  dwIndex:       Integer;
begin

  // Resource protection
  try
     // Clear all filters from outbound list (do not worry about removing them)
     for dwIndex:=Pred(FOutbound.Count) downto 0 do FreeFilter(FOutbound[dwIndex]);
     // Clear all filters from inbound list (do not worry about removing them)
     for dwIndex:=Pred(FInbound.Count) downto 0 do FreeFilter(FInbound[dwIndex]);
  finally
     // Clear the lists
     FOutbound.Clear;
     FInbound.Clear;
     // Unbind the interface handle (if active)
     if FActive then PfUnBindInterface(FHandle);
     // Delete the interface handle
     PfDeleteInterface(FHandle);
  end;

end;

constructor TIpFilter.Create;
begin

  // Perform inherited
  inherited Create;

  // Create internal lists
  FActive:=False;
  FOutbound:=TList.Create;
  FInbound:=TList.Create;

  // Startup
  Win32Check(Startup);

end;

destructor TIpFilter.Destroy;
begin

  // Resource protection
  try
     // Cleanup lists and delete the interface
     Cleanup;
     // Free the lists
     FreeAndNil(FOutbound);
     FreeAndNil(FInbound);
  finally
     // Perform inherited
     inherited Destroy;
  end;

end;

function StrToIp(lpszIP: PChar; lpipAddr: PIpBytes): PIpBytes;
var  dwAddr:     LongWord;
     pheAddr:    PHostEnt;
begin

  // Check for common settings
  if Assigned(lpszIP) and (StrIComp(lpszIP, IP_LOCALHOST) = 0) then
     // Get the local ip address
     GetLocalIPAddr(lpipAddr)
  else
  begin
     // Check assignment
     if Assigned(lpszIp) then
     begin
        // Check mask all
        if (StrIComp(lpszIP, IP_MASKALL) = 0) then
           dwAddr:=0
        // Check mask none
        else if (StrIComp(lpszIP, IP_MASKNONE) = 0) then
           dwAddr:=INADDR_NONE
        else
        begin
           // Attempt . notation conversion
           dwAddr:=inet_addr(lpszIP);
           // Check conversion (IP_MASKNONE will have already been handled)
           if (dwAddr = LongWord(INADDR_NONE)) then
           begin
              // Attempt host name conversion
              pheAddr:=gethostbyname(lpszIP);
              // Check conversion
              if Assigned(pheAddr) then
                 // Get the ip address
                 Move(pheAddr^.h_addr_list^^, dwAddr, SizeOf(LongWord))
              else
                 // Failed to get the ip address, use none
                 dwAddr:=INADDR_NONE;
           end;
        end
     end
     else
        // Convert IP_MASKALL
        dwAddr:=inet_addr(IP_MASKALL);
     // Move the longword into the byte array
     Move(dwAddr, lpipAddr^, SizeOf(LongWord));
  end;

  // Result is the pointer to passed ip address buffer
  result:=lpipAddr;

end;

function GetLocalIPAddr(lpipAddr: PIpBytes): Boolean;
var  lpszLocal:  Array [0..255] of Char;
     pheAddr:    PHostEnt;
begin

  // Get the host name
  if (gethostname(lpszLocal, SizeOf(lpszLocal)) = 0) then
  begin
     // Get the host ent structure
     pheAddr:=gethostbyname(lpszLocal);
     if Assigned(pheAddr) then
     begin
        // Get the ip address
        Move(pheAddr^.h_addr_list^^, lpipAddr^, 4);
        result:=True;
     end
     else
        // Failure
        result:=False;
  end
  else
     // Failure
     result:=False;

end;

initialization

  // Initialize winsock so we can get the local ip address and call winsock functions
  WSAStartup(MakeWord(1, 1), wsaData);

finalization

  // Cleanup
  WSACleanup;

end.

