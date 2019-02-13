unit ETradeDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TETradeDoc = class(TComponent)
  private

  protected

  public

  published

  end;

procedure Register;

implementation
uses ExchangeFile;

procedure Register;
begin
  RegisterComponents('TradeEquipment',[TETradeDoc]);
end;

end.
