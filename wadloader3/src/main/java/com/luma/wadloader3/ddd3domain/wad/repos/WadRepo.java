package com.luma.wadloader3.ddd3domain.wad.repos;

import com.luma.wadloader3.ddd3domain.wad.model.Wad;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface WadRepo extends JpaRepository<Wad, Integer> {
    boolean existsByName(String name);
}
