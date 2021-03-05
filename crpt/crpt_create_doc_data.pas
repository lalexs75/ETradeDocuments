{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020-2021 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit crpt_create_doc_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TCRPTCreateDocumentData }

  TCRPTCreateDocumentData =  class(TJSONSerializationObject)
  private
    FDocumentFormat: string;
    FDocumentType: string;
    FProductDocument: string;
    FSignature: string;
    procedure SetDocumentFormat(AValue: string);
    procedure SetDocumentType(AValue: string);
    procedure SetProductDocument(AValue: string);
    procedure SetSignature(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadProductDocument(AStream:TStream);
    procedure LoadSignature(AStream:TStream);
  published
    property DocumentFormat:string read FDocumentFormat write SetDocumentFormat;
    property ProductDocument:string read FProductDocument write SetProductDocument;
    property DocumentType:string read FDocumentType write SetDocumentType;
    property Signature:string read FSignature write SetSignature;
  end;

implementation
uses base64;

function EncodeStringBase64W(const S:TStream):String;
var
  OutStream : TStringStream;
  Encoder   : TBase64EncodingStream;
begin
  if not Assigned(S) then
    Exit('');

  OutStream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(OutStream);
    try
      Encoder.CopyFrom(S, S.Size);
    finally
      Encoder.Free;
    end;
    Result:=OutStream.DataString;
  finally
    OutStream.free;
  end;
end;


{ TCRPTCreateDocumentData }

procedure TCRPTCreateDocumentData.SetDocumentFormat(AValue: string);
begin
  if FDocumentFormat=AValue then Exit;
  CheckLockupValue('DocumentFormat', AValue);
  FDocumentFormat:=AValue;
  ModifiedProperty('DocumentFormat');
end;

procedure TCRPTCreateDocumentData.SetDocumentType(AValue: string);
begin
  if FDocumentType=AValue then Exit;
  FDocumentType:=AValue;
  ModifiedProperty('DocumentType');
end;

procedure TCRPTCreateDocumentData.SetProductDocument(AValue: string);
begin
  if FProductDocument=AValue then Exit;
  FProductDocument:=AValue;
  ModifiedProperty('ProductDocument');
end;

procedure TCRPTCreateDocumentData.SetSignature(AValue: string);
begin
  if FSignature=AValue then Exit;
  FSignature:=AValue;
  ModifiedProperty('Signature');
end;

procedure TCRPTCreateDocumentData.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('DocumentFormat', 'document_format', [], '', -1, -1);
    P.ValidList.Add('MANUAL');
    P.ValidList.Add('XML');
    P.ValidList.Add('CSV');
  RegisterProperty('ProductDocument', 'product_document', [], '', -1, -1);
  RegisterProperty('DocumentType', 'type', [], '', -1, -1);
  RegisterProperty('Signature', 'signature', [], '', -1, -1);
end;

procedure TCRPTCreateDocumentData.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

constructor TCRPTCreateDocumentData.Create;
begin
  inherited Create;
end;

destructor TCRPTCreateDocumentData.Destroy;
begin
  inherited Destroy;
end;

procedure TCRPTCreateDocumentData.LoadProductDocument(AStream: TStream);
begin
  ProductDocument:=EncodeStringBase64W(AStream)
end;

procedure TCRPTCreateDocumentData.LoadSignature(AStream: TStream);
begin
  Signature:=EncodeStringBase64W(AStream)
end;

end.

