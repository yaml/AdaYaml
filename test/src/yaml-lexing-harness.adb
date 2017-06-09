with Yaml.Lexing.Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Yaml.Lexing.Harness is
   procedure Run is new AUnit.Run.Test_Runner (Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Yaml.Lexing.Harness;
