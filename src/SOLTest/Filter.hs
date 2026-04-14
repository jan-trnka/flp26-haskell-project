-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
--
-- FLP: Implement this function using @matchesAny@ and @matchesCriterion@.
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
filterTests spec tests = 
  let included = if null (fsIncludes spec)                          -- No include criteria means include all tests.
                 then tests
                 else filter (matchesAny (fsIncludes spec)) tests   -- Filter tests by include criteria.
      excluded = filter (matchesAny (fsExcludes spec)) included     -- Filter the included tests by exclude criteria.
      selected = filter (`notElem` excluded) included               -- Selected tests are included but not excluded.
  in (selected, excluded)

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny criteria test =
  any (matchesCriterion test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
--
-- FLP: Implement this function. If you're not implementing the regex matching
-- bonus extension, you can either remove the first argument and update the usages,
-- or you can simply ignore the value.
matchesCriterion :: TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion test criterion = do
  case criterion of
    -- @ByAny@ matches if the criterion matches the test name, category, or tag.
    ByAny crit -> tcdName test == crit || tcdCategory test == crit || crit `elem` tcdTags test
    -- @ByCategory@ matches if the criterion matches the test category.
    ByCategory cat -> tcdCategory test == cat
    -- @ByTag@ matches if the criterion tag is in the test's tags.
    ByTag tag -> tag `elem` tcdTags test

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
