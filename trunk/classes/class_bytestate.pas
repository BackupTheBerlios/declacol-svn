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
// Statemachine um eine Bytestrom zu analysieren
////////////////////////////////////////////////////////////////////////////////
unit class_bytestate;

interface
uses unit_typedefs,windows;

//Einige Konstanten
const
     //Maximale anzahl an Transistionen pro State;
     MAX_TRANSITIONS = 4;


type
//Callback bei einer Transistion
TTransitionCallback = procedure(SourceID : unsigned32; TargetID : unsigned32; TriggerByte : unsigned8) of Object;
TStateCallback      = procedure(StateID  : unsigned32; TriggerByte : unsigned8) of Object;

//Eine Transisition
type TTransition = record
     //Welche Byte lösen die Transistion aus
     Trigger  : set of 0..High(unsigned8);
     //Wohin geht es
     TargetID : unsigned32;
     //Callback
     Callback : TTransitionCallback;
end;

//Definition für einen State
type TState = record
     ID              : unsigned32;
     Name            : string[32];
     Transitions     : Array [0..MAX_TRANSITIONS - 1] of TTransition;
     Callback        : TStateCallback;

     //Timeout berücksichtigen
     Timestamp       : unsigned32;
     Timeout         : unsigned32;
     TimeTransition  : unsigned32;
     TimeCallback    : TTransitionCallback;
     
end;

//Unsere eigentliche Statemachine
type TByteState = class (TObject)
     private
            aStates  : array of TState;

            u32State  : unsigned32;
            u32StateCount : unsigned32;



            function  FindState (ID : unsigned32; var State : TState):Boolean;
            function  FindStateIndex(Key : unsigned32; var StateIndex : unsigned32):Boolean;
            procedure CopyState(Source : TState; var Target : TState);

            function GetCurrentState():TState;
     protected
     public
            constructor Create();
            destructor  Free();

            //Verwaltung der States
            function  AddState  (State : TState):unsigned32;
            function  GetState  (ID : unsigned32):TState;
            procedure SetState  (ID : unsigned32; Value :TState);

            //Abarbeiten der States
            //Die Statemachine resetten
            function Reset(FirstID : unsigned32 = ID_NONE):Boolean;
            //Das nächste Byte verarbeiten
            function  Next (Input : unsigned8):Boolean;

            //Timeout berücksichtigen und verarbeiten
            procedure Heartbeat();

            property CurrentID    : unsigned32 read u32State write u32State;
            property CurrentState : TState read GetCurrentState;

            property Count : unsigned32 read u32StateCount;
end;

implementation



constructor TByteState.Create();
begin
end;

destructor  TByteState.Free();
begin
end;


////////////////////////////////////////////////////////////////////////////////
//die Verarbeitung der Statemachine
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//Maschine zurücksetzen
function TByteState.Reset(FirstID : unsigned32):Boolean;
var
   u32Index : unsigned32;
begin
     u32Index:=0;

     //Wird keine StartID übergeben nehmen wir den ersten State im Array
     if (FirstID <> ID_NONE) then
        begin
             Self.FindStateIndex(FirstID,u32Index);
        end;

     Self.u32State:=u32Index;
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen State verarbeiten
function  TByteState.Next (Input : unsigned8):Boolean;
var
   u32Index : unsigned32;
begin
     result:=FALSE;


     //Index OK ?
     if (Self.u32State < Self.Count) then
        begin
             //StateCallback vor einer Transition
             if (assigned(Self.aStates[Self.u32State].Callback)=TRUE) then
                begin
                     Self.aStates[Self.u32State].Callback( Self.aStates[Self.u32State].ID,Input);
                end;

             //Alle Transistionen prüfen
             u32Index := 0;
             while (u32Index < MAX_TRANSITIONS) do
                   begin
                        //Ein Byte in einem Transitionpool ?
                        if (input in Self.aStates[Self.u32State].Transitions[u32Index].Trigger) then
                           begin
                                //TransitionCallback aufrufen
                                if (assigned(Self.aStates[Self.u32State].Transitions[u32Index].CallBack) = TRUE) then
                                   begin
                                        //Und los
                                        Self.aStates[Self.u32State].Transitions[u32Index].CallBack(Self.u32State,Self.aStates[Self.u32State].Transitions[u32Index].TargetID,Input);
                                   end;

                                //StateIndex anpassen
                                Self.u32State:=Self.aStates[Self.u32State].Transitions[u32Index].TargetID;

                                //Statetimeout setzen
                                Self.aStates[Self.u32State].Timestamp:=GetTickCount() + Self.aStates[Self.u32State].Timeout;

                                //Fertig
                                result:=TRUE;
                                u32Index:=MAX_TRANSITIONS;
                           end;

                        inc(u32Index);
                   end;
         end;
end;

//Mit dieser Funktion kann ein Timeout in der Statemachine berücksichtigt werden.
//Je nach zeitlicher Auflösung des gesetzten StateTimeouts sollte Heartbeat aufgerufen werden
procedure TByteState.Heartbeat();
begin
     //Index OK ?
     if (Self.u32State < Self.Count) then
        begin
             //haben wir einen Timeout ?
             if (Self.aStates[u32State].Timestamp < GetTickCount()) then
                begin
                     //TransitionCallback aufrufen
                     if (assigned(Self.aStates[Self.u32State].TimeCallBack) = TRUE) then
                        begin
                             Self.aStates[Self.u32State].TimeCallBack(Self.u32State,Self.aStates[Self.u32State].TimeTransition,0);
                        end;
                end;

             //StateIndex anpassen
             Self.u32State:=Self.aStates[Self.u32State].TimeTransition;

             //Statetimeout setzen
              Self.aStates[Self.u32State].Timestamp:=GetTickCount() + Self.aStates[Self.u32State].Timeout;
         end;
end;


////////////////////////////////////////////////////////////////////////////////
//Einen State zufügen
function  TByteState.AddState (State : TState):unsigned32;
begin
     result:=Length(Self.aStates);

     SetLength(Self.aStates,result + 1);

     Self.CopyState(State,Self.aStates[result]);

     inc(Self.u32StateCount);
end;

////////////////////////////////////////////////////////////////////////////////
//Einen State suchen
function  TByteState.FindState(ID : unsigned32; var State : TState):Boolean;
var
   u32Index : unsigned32;
begin
     result:=FALSE;
     u32Index := 0;

     while (u32Index < Self.Count) do
           begin
                if (Self.aStates[u32Index].ID = ID) then
                   begin
                        Self.CopyState(Self.aStates[u32Index],State);
                        result:=TRUE;
                   end;

                inc(u32Index);
           end;
end;

function  TByteState.FindStateIndex(Key : unsigned32; var StateIndex : unsigned32):Boolean;
var
   u32Index : unsigned32;
begin
     result:=FALSE;
     u32Index := 0;

     while (u32Index < Self.Count) do
           begin
                if (Self.aStates[u32Index].ID = Key) then
                   begin
                        StateIndex:=u32Index;
                        result:=TRUE;
                   end;

                inc(u32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen State auf einen anderen kopieren
procedure TByteState.CopyState(Source : TState; var Target : TState);
var
   u32Index : unsigned32;
begin
     Target.ID   :=Source.ID;
     Target.Name :=Source.Name;

     for u32Index:=0 to MAX_TRANSITIONS - 1 do
           begin
                Target.Transitions[u32Index]:=Source.Transitions[u32Index];
           end;

     Target.Callback:=Source.Callback;
end;


////////////////////////////////////////////////////////////////////////////////
//Getter und Setter
////////////////////////////////////////////////////////////////////////////////
function  TByteState.GetState(ID : unsigned32):TState;
begin
     if (Self.FindState(ID,Result)=FALSE) then
        begin
             Result.ID:=ID_NONE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TByteState.SetState(ID : unsigned32; Value :TState);
var
   u32Index : unsigned32;
begin
     //Wenn der State schon existiert seine Werte überschreiben
     if (Self.FindStateIndex(ID,u32Index) = TRUE) then
        begin
             Self.CopyState(Value,Self.aStates[u32Index]);
        end
     else
        begin
             //Ansonsten einen neuen zufügen
             Self.AddState(Value);
        end;
end;

function TByteState.GetCurrentState():TState;
begin
     result:=Self.GetState(Self.u32State);
end;

end.
