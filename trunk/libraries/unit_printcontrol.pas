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

///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
/// Einfache Unit um ein wenig mit dem Drucker zu spielen
///
/// (c) 2004 Borg@Sven-of-Nine.de
///
///
///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
unit unit_printcontrol;

interface

uses
  Classes,SysUtils,Windows,Printers,Winspool;

type
  TPrinterControl = class(TComponent)
  private
    { Private-Deklarationen }
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
  published
    { Published-Deklarationen }

    function GetStandardPrinter():String;

    function GetPrinterHandle(PrinterName:string):THandle;
    function GetPrinterStatus(PHandle:THandle):Integer;
    function SetPrinterStatus(PHandle:THandle;Status:integer):Boolean;

    function GetNumberOfJobs(PHandle:THandle):Integer;
    function GetJobStatus(PHandle:THandle;Job:integer):Integer;
    function GetJobName(PHandle:THandle;Job:integer):string;
    function GetJobSender(PHandle:THandle;Job:integer):string;
    function GetJobHandle(PHandle:THandle;Job:integer):THandle;

    function ResumeNetworkJobs():Boolean;
    function ResumeLocalJobs():Boolean;

    function ResumePrinter(h:THandle):Boolean;
    function PausePrinter(h:THandle):Boolean;

    function SetJobStatus(PHandle:THandle;Job:integer;Status:integer):Boolean;

    function GetPCId():string;
    
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('So9', [TPrinterControl]);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
function TPrinterControl.GetStandardPrinter():String;
var
   s:string;
   prn:TPrinter;
var
  ResStr: array[0..255] of Char;
begin
     s:='NA';
     prn:=printer();

     //Drucker installiert ?
     if (prn.printers.count>0) then
        begin
             //Druckername holen
             GetProfileString('Windows', 'device', '', ResStr, 255);
             s := Trim(StrPas(ResStr));
        end;

     //Druckernamen formatieren
     if (s='') then s:='NA';
     s:=copy(s,0,Pos(',',s)-1);
     Result:=trim(s);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.GetPrinterHandle(PrinterName:string):THandle;
var
   P:array[0..255]of Char;
begin
     StrPCopy(P,PrinterName);
     if OpenPrinter(@P, Result, nil)=FALSE then Result:=0;
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.GetNumberOfJobs(PHandle:THandle):integer;
type
   TPrinterInfo=Printer_Info_2;
   PPrinterInfo=^TPrinterInfo;
var
   BytesNeeded:Cardinal;
   PInfo:PPrinterInfo;
   ok:boolean;
begin
     ok:=TRUE;
     result:=-1;

     //Speicherbedarf lesen
     GetPrinter(PHandle,2,nil,0,@BytesNeeded);
     PInfo:=AllocMem(BytesNeeded);

     //Daten holen
     ok:=GetPrinter(PHandle,2,PInfo,BytesNeeded,@BytesNeeded);

     if (ok=true) then
        begin
             Result:=integer(PInfo.cJobs);
        end
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.GetJobStatus(PHandle:THandle;Job:integer):integer;
type
   TJobs  = array [0..1000] of JOB_INFO_2;
   PJobs = ^TJobs;
var
   bytesNeeded, numJobs: Cardinal;
   pJ: PJobs;
begin
     dec(Job);
     EnumJobs(PHandle, 0, 1000, 2, nil, 0, bytesNeeded, numJobs);

     pJ := AllocMem(bytesNeeded);

     if EnumJobs(PHandle, 0, 1000, 2, pJ, bytesNeeded, bytesNeeded, numJobs)=FALSE then numJobs:=0;

     if (Job>NumJobs) then Result:=-1 else Result:=integer(pJ^[Job].Status);
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.SetJobStatus(PHandle:THandle;Job:integer;Status:Integer):Boolean;
type
   TJobs  = array [0..1000] of JOB_INFO_2;
   PJobs = ^TJobs;
var
   bytesNeeded, numJobs: Cardinal;
   pJ: PJobs;
begin
     dec(Job);
     EnumJobs(PHandle, 0, 1000, 2, nil, 0, bytesNeeded, numJobs);

     pJ := AllocMem(bytesNeeded);

     if EnumJobs(PHandle, 0, 1000, 2, pJ, bytesNeeded, bytesNeeded, numJobs)=FALSE then numJobs:=0;

     Result:=SetJob(PHandle,pJ^[Job].JobID,2,@pJ^[Job],Status);
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.GetJobName(PHandle:THandle;Job:integer):string;
type
   TJobs  = array [0..1000] of JOB_INFO_1;
   PJobs = ^TJobs;
var
   bytesNeeded, numJobs: Cardinal;
   pJ: PJobs;
begin
     Dec(Job);
     EnumJobs(PHandle, 0, 1000, 2, nil, 0, bytesNeeded, numJobs);

     pJ := AllocMem(bytesNeeded);

     if EnumJobs(PHandle, 0, 1000, 1, pJ, bytesNeeded, bytesNeeded, numJobs)=FALSE then numJobs:=0;

     if (Job<=numJobs) then
        begin
             Result:=pJ^[Job].pDocument;
        end
     else
        begin
             Result:='NA';
        end;
end;
//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.GetJobSender(PHandle:THandle;Job:integer):string;
type
   TJobs  = array [0..1000] of JOB_INFO_1;
   PJobs = ^TJobs;
var
   bytesNeeded, numJobs: Cardinal;
   pJ: PJobs;
   z:integer;
begin
     Dec(Job);
     EnumJobs(PHandle, 0, 1000, 2, nil, 0, bytesNeeded, numJobs);

     pJ := AllocMem(bytesNeeded);

     if EnumJobs(PHandle, 0, 1000, 1, pJ, bytesNeeded, bytesNeeded, numJobs)=FALSE then numJobs:=0;

     if (Job<=numJobs) then
        begin
             Result:=pJ^[Job].pMachineName;
        end
     else
        begin
             Result:='NA';
        end;
   for z:=0 to Length(Result) do
       begin
            if Result[z]='\' then Result[z]:=' ';
       end;
   Result:=trim (Result);

end;


//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.GetJobHandle(PHandle:THandle;Job:integer):tHandle;
type
   TJobs  = array [0..1000] of JOB_INFO_1;
   PJobs = ^TJobs;
var
   bytesNeeded, numJobs: Cardinal;
   pJ: PJobs;
begin
     Dec(Job);
     EnumJobs(PHandle, 0, 1000, 2, nil, 0, bytesNeeded, numJobs);

     pJ := AllocMem(bytesNeeded);

     if EnumJobs(PHandle, 0, 1000, 1, pJ, bytesNeeded, bytesNeeded, numJobs)=FALSE then numJobs:=0;

     if (Job<=numJobs) then
        begin
             Result:=pJ^[Job].JobID;
        end
     else
        begin
             Result:=0;
        end;
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.GetPrinterStatus(PHandle:THandle):integer;
type
   TPrinterInfo=Printer_Info_2;
   PPrinterInfo=^TPrinterInfo;
var
   BytesNeeded:Cardinal;
   PInfo:PPrinterInfo;
   ok:boolean;
begin
     ok:=TRUE;
     result:=-1;

     //Speicherbedarf lesen
     GetPrinter(PHandle,2,nil,0,@BytesNeeded);
     PInfo:=AllocMem(BytesNeeded);

     //Daten holen
     ok:=GetPrinter(PHandle,2,PInfo,BytesNeeded,@BytesNeeded);

     if (ok=true) then
        begin
             Result:=integer(PInfo.Status);
        end
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////

function TPrinterControl.SetPrinterStatus(PHandle:THandle;Status:integer):Boolean;
type
   TPrinterInfo=Printer_Info_2;
   PPrinterInfo=^TPrinterInfo;
var
   BytesNeeded:Cardinal;
   PInfo:PPrinterInfo;
   ok:boolean;
begin
     ok:=TRUE;

     //Speicherbedarf lesen
     GetPrinter(PHandle,2,nil,0,@BytesNeeded);
     PInfo:=AllocMem(BytesNeeded);

     //Daten holen
     ok:=GetPrinter(PHandle,2,PInfo,BytesNeeded,@BytesNeeded);

//     PInfo.Status:=Status;

     //Status schreiben
     Result:=SetPrinter(PHandle,0,PInfo,Status);
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////
function TPrinterControl.GetPCId():string;
var
   pcName : pchar;   // Holds the computer name
   dwSize : dword;   // Size of the buffer holding the name
   z:integer;
begin

   try
      dwSize := MAX_COMPUTERNAME_LENGTH + 1;
      pcName := strAlloc( dwSize );

      if getComputerName( pcName, dwSize )=FALSE then pcName:='?';
   finally
   end;

   for z:=0 to dwSize do
       begin
            if pcName[z]='\' then pcName[z]:=' ';
       end;

   Result:=string( trim (pcName) );
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
function TPrinterControl.ResumeNetworkJobs():Boolean;
var
   h:THandle;
   z:integer;
begin
     h:=GetPrinterHandle(GetStandardPrinter());
     z:=GetNumberOfJobs(h);


     if (z>0) then
        begin
             //Alle Jobs durchgehen
             for z:=1 to GetNumberOfJobs(h) do
                 begin
                      //Nur Netzwerk Jobs
                      if (GetJobSender(h,z)<>GetPCID()) then
                         begin
                              Result:=SetJobStatus(h,z,JOB_CONTROL_RESUME);
                         end;
                 end;
        end;
     Result:=TRUE;
     ClosePrinter(h);
end;
//////////////////////////////////////////////////////////////////////////////////////////////////////
function TPrinterControl.ResumeLocalJobs():Boolean;
var
   h:THandle;
   z:integer;
begin
     h:=GetPrinterHandle(GetStandardPrinter());
     z:=GetNumberOfJobs(h);


     if (z>0) then
        begin
             //Alle Jobs durchgehen
             for z:=1 to GetNumberOfJobs(h) do
                 begin
                      //Nur Netzwerk Jobs
                      if (GetJobSender(h,z)=GetPCID()) then
                         begin
                              Result:=SetJobStatus(h,z,JOB_CONTROL_RESUME);
                         end;
                 end;
        end;
     ClosePrinter(h);
end;
//////////////////////////////////////////////////////////////////////////////////////////////////////
function TPrinterControl.ResumePrinter(h:THandle):Boolean;
begin

     if (GetPrinterStatus(h)=PRINTER_STATUS_PAUSED) then
        begin
             Result:=SetPrinterStatus(h,PRINTER_CONTROL_RESUME);
        end;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
function TPrinterControl.PausePrinter(h:THandle):Boolean;
begin

     if (GetPrinterStatus(h)<>PRINTER_STATUS_PAUSED) then
        begin
             Result:=SetPrinterStatus(h,PRINTER_CONTROL_PAUSE);
        end;
end;
//////////////////////////////////////////////////////////////////////////////////////////////////////


end.
