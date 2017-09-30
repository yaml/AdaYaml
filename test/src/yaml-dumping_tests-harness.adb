--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Dumping_Tests.Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Yaml.Dumping_Tests.Harness is
   procedure Run is new AUnit.Run.Test_Runner (Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Run (Reporter);
end Yaml.Dumping_Tests.Harness;
