package ctx

class PromptPreference(val prompt: String)

object Greeter:
  def greet(message: String)(using pref: PromptPreference): Unit =
    println(s"${pref.prompt} > $message")

object DefaultPref:
  given prompt: PromptPreference = PromptPreference("###")

object Promot extends App:
  import DefaultPref.prompt
  Greeter.greet("hello there")
