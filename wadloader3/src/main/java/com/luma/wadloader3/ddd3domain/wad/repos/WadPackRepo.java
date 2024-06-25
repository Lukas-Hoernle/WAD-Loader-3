package com.luma.wadloader3.ddd3domain.wad.repos;

import com.luma.wadloader3.ddd3domain.wad.model.WadPack;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface WadPackRepo extends JpaRepository<WadPack, Integer> {

    boolean existsByName(String name);
}
