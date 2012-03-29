def load_fixture name
  {
    :input  => File.read("fixtures/#{name}.selfml"),
    :output => File.read("fixtures/output/#{name}.selfml")
  }
end
