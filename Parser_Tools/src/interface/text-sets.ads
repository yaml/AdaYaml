with Ada.Containers.Hashed_Sets;

package Text.Sets is new Ada.Containers.Hashed_Sets
  (Reference, Hash, Text."=");
