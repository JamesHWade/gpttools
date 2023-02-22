# API checking fails with missing, inactive, or badly formatted key

    Code
      check_api()
    Message <rlang_message>
      Checking API key using OPENAI_API_KEY environment variable...
    Message <cliMessage>
      v API key found and matches the expected format.
      x API key found but call was unsuccessful.
      i Attempted to use API key: 38a5****************************2d60

---

    Code
      check_api()
    Message <rlang_message>
      Checking API key using OPENAI_API_KEY environment variable...
    Message <cliMessage>
      ! OPENAI_API_KEY is not set.

---

    Code
      check_api()
    Message <rlang_message>
      Checking API key using OPENAI_API_KEY environment variable...
    Message <cliMessage>
      x API key not found or is not formatted correctly.
      i Attempted to validate key: <hidden> (too short to obscure)
      i Generate a key at <https://beta.openai.com/account/api-keys>

# API checking works, assumes OPENAI_API_KEY is set

    Code
      check_api()
    Message <rlang_message>
      Checking API key using OPENAI_API_KEY environment variable...
    Message <cliMessage>
      ! OPENAI_API_KEY is not set.

---

    Code
      check_api()
    Message <rlang_message>
      Checking API key using OPENAI_API_KEY environment variable...
    Message <cliMessage>
      ! OPENAI_API_KEY is not set.

---

    Code
      check_api()
    Message <rlang_message>
      Checking API key using OPENAI_API_KEY environment variable...
    Message <cliMessage>
      v API key found and matches the expected format.
      x API key found but call was unsuccessful.
      i Attempted to use API key: 38a5****************************2d60

# API key validation works

    Code
      check_api_key(sample_key)
    Message <cliMessage>
      v API key found and matches the expected format.

---

    Code
      check_api_key("1234")
    Message <cliMessage>
      x API key not found or is not formatted correctly.
      i Attempted to validate key: <hidden> (too short to obscure)
      i Generate a key at <https://beta.openai.com/account/api-keys>

---

    Code
      check_api_key("")
    Message <cliMessage>
      ! OPENAI_API_KEY is not set.

# API connection checking works

    Code
      check_api_connection(sample_key)
    Message <cliMessage>
      v API key found and matches the expected format.
      x API key found but call was unsuccessful.
      i Attempted to use API key: 38a5****************************2d60

---

    Code
      check_api_connection("")
    Message <cliMessage>
      ! OPENAI_API_KEY is not set.

# API connection can return true

    Code
      check_api_connection(Sys.getenv("OPENAI_API_KEY"))
    Message <cliMessage>
      ! OPENAI_API_KEY is not set.

