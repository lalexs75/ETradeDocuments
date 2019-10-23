{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании "Приказ ФНС РФ от 19.12.2018 N ММВ-7-15/820@"

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

unit InvoceExchangeFile_5_02;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractExchangeFileUnit, InvoceExchangeFile, InvoceDocument;

type

  { TExchangeFile5_02 }

  TExchangeFile5_02 = class(TAbstractExchangeFile)   //%Таблица 5.1
  private
    FDocument: TInvoceDocument;
    FParticipantsInformation: TParticipantsInformation;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ParticipantsInformation:TParticipantsInformation read FParticipantsInformation;
    property Document:TInvoceDocument read FDocument;
  end;

implementation

{ TExchangeFile }

procedure TExchangeFile5_02.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('ParticipantsInformation', 'СвУчДокОбор', 'О', 'Сведения об участниках электронного документооборота', -1, -1);
  RegisterProperty('Document', 'Документ', 'О', 'Счет-фактура, или документ об отгрузке товаров (выполнении работ), передаче имущественных прав (документ об оказании услуг), включающий в себя счет-фактуру (информация продавца), или документ об отгрузке товаров (выполнении работ), передаче имущественных прав (документ об оказании услуг) (информация продавца)', -1, -1);
end;

procedure TExchangeFile5_02.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FParticipantsInformation:=TParticipantsInformation.Create;
  FDocument:=TInvoceDocument.Create;
end;

destructor TExchangeFile5_02.Destroy;
begin
  FreeAndNil(FParticipantsInformation);
  FreeAndNil(FDocument);
  inherited Destroy;
end;

end.

